package ducttape.exec

import collection._
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.workflow.HyperWorkflow
import ducttape.util.BashException
import grizzled.slf4j.Logging

// workflow used for viz
class Executor(val dirs: DirectoryArchitect,
//               versions: WorkflowVersioner,
               val packageVersioner: PackageVersioner,
               val workflow: HyperWorkflow,
               val plannedVertices: Set[(String,Realization)],
               val alreadyDone: Set[(String,Realization)],
               val todo: Set[(String,Realization)],
               observers: Seq[ExecutionObserver] = Nil) extends UnpackedDagVisitor with Logging {
  import ducttape.viz.WorkflowViz
  
  val submitter = new Submitter(workflow.submitters)

  observers.foreach(_.init(this))

  override def visit(task: RealTask) {
    if (todo( (task.name, task.realization) )) {
      val taskEnv = new FullTaskEnvironment(dirs, packageVersioner, task)
      System.err.println("Running %s in %s".format(task, taskEnv.where.getAbsolutePath))
      try {  
        observers.foreach(_.begin(this, task))
        taskEnv.where.mkdirs
        if (!taskEnv.where.exists) {
          observers.foreach(_.fail(this, task))
          throw new BashException("Could not make directory: " + taskEnv.where.getAbsolutePath)
        }
        
        debug("Environment for %s is %s".format(task, taskEnv.env))
  
        // the "run" action of the submitter will throw if the exit code is non-zero
        submitter.run(taskEnv)
        
        if (!CompletionChecker.isComplete(taskEnv)) {
          val msg = "Task completed, but did not satisfy post-conditions. Check output: " + taskEnv.where.getAbsolutePath
          System.err.println("Failed %s: %s".format(task, msg))
          observers.foreach(_.fail(this, task))
          throw new BashException(msg)
        }
      } finally {
        // TODO: Factor out into listener/callback?
         taskEnv.lockFile.delete()
      }
      System.err.println("Completed %s".format(task))
    }
    observers.foreach(_.complete(this, task))
  }
}