package ducttape.exec

import collection._
import ducttape.versioner.WorkflowVersioner
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.workflow.HyperWorkflow
import ducttape.util.BashException

// workflow used for viz
class Executor(dirs: DirectoryArchitect,
//               versions: WorkflowVersioner,
               packageVersioner: PackageVersioner,
               workflow: HyperWorkflow,
               plannedVertices: Set[(String,Realization)],
               alreadyDone: Set[(String,Realization)],
               todo: Set[(String,Realization)]) extends UnpackedDagVisitor {
  import ducttape.viz.WorkflowViz
  
  val submitter = new Submitter(workflow.submitters)

  // TODO: Construct set elsewhere?
  val completed = new mutable.HashSet[(String,Realization)]
  val running = new mutable.HashSet[(String,Realization)]
  val failed = new mutable.HashSet[(String,Realization)]

  // TODO: Move all dot-related things to an instrumentation class
  dirs.xdotFile.synchronized {
    completed ++= alreadyDone
    Files.write(WorkflowViz.toGraphViz(workflow, plannedVertices, completed, running, failed), dirs.xdotFile)
  }

  override def visit(task: RealTask) {
    if(todo((task.name, task.realization))) {
      val taskEnv = new FullTaskEnvironment(dirs, packageVersioner, task)
      println("Running %s in %s".format(task.name, taskEnv.where.getAbsolutePath))

      dirs.xdotFile.synchronized {
        running += ((task.name, task.realization))
        Files.write(WorkflowViz.toGraphViz(workflow, plannedVertices, completed, running, failed), dirs.xdotFile)
      }

      taskEnv.where.mkdirs
      if(!taskEnv.where.exists) {
        failed += ((task.name, task.realization))
        running -= ((task.name, task.realization))
        dirs.xdotFile.synchronized {
          Files.write(WorkflowViz.toGraphViz(workflow, plannedVertices, completed, running, failed), dirs.xdotFile)
        }
        throw new BashException("Could not make directory: " + taskEnv.where.getAbsolutePath)
      }

      // the "run" action of the submitter will throw if the exit code is non-zero
      submitter.run(taskEnv)
    }
    completed += ((task.name, task.realization))
    running -= ((task.name, task.realization))
    dirs.xdotFile.synchronized {
      Files.write(WorkflowViz.toGraphViz(workflow, plannedVertices, completed, running, failed), dirs.xdotFile)
    }
  }
}