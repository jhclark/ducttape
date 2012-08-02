package ducttape.exec

import collection._
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.PlanPolicy
import ducttape.util.BashException
import grizzled.slf4j.Logging

// workflow used for viz
class Executor(val dirs: DirectoryArchitect,
//               versions: WorkflowVersioner,
               val packageVersioner: PackageVersioner,
               val planPolicy: PlanPolicy,
               val locker: LockManager,
               val workflow: HyperWorkflow,
               val alreadyDone: Set[(String,Realization)],
               val todo: Set[(String,Realization)],
               observers: Seq[ExecutionObserver] = Nil) extends UnpackedDagVisitor with Logging {
  import ducttape.viz.WorkflowViz
  
  val submitter = new Submitter(workflow.submitters)

  observers.foreach(_.init(this))

  override def visit(task: RealTask) {
    if (todo( (task.name, task.realization) )) {
      
      val taskEnv = new FullTaskEnvironment(dirs, packageVersioner, task)
      // first, acquire a lock
      System.err.println("Acquiring lock for %s".format(task))
      locker.acquireLock(taskEnv)
      
      taskEnv.fullSymlink match {
        case None => ;
        case Some(link) => {
          Files.symlink(taskEnv.where, link)
        }
      }

      // Note: If we just acquired the lock,
      // the LockManager will have just written ducttape_version.txt for us as well.
      // and moved any previous partial output
      
      try {
        // this task could have been completed by another ducttape process
        // while we were waiting on the lock
        if (!CompletionChecker.isComplete(taskEnv)) {
          
          System.err.println("Running %s in %s".format(task, taskEnv.where.getAbsolutePath))
          observers.foreach(_.begin(this, taskEnv))

          Files.mkdirs(taskEnv.where)          
          debug("Environment for %s is %s".format(task, taskEnv.env))
    
          // the "run" action of the submitter will throw if the exit code is non-zero
          submitter.run(taskEnv)
          
          if (!CompletionChecker.isComplete(taskEnv)) {
            throw new BashException("Task completed, but did not satisfy post-conditions. Check output: " + taskEnv.where.getAbsolutePath)
          }
        }
      } catch {
        case t: Throwable => {
          System.err.println("Failed %s: %s".format(task, t.getMessage))
          observers.foreach(_.fail(this, taskEnv))
          throw t
        }
      } finally {
        locker.releaseLock(taskEnv)
      }
      System.err.println("Completed %s".format(task))
      observers.foreach(_.succeed(this, taskEnv))
    } else {
      val taskEnv = new TaskEnvironment(dirs, task)
      observers.foreach(_.skip(this, taskEnv))
    }
  }
}
