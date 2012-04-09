package ducttape.exec

import collection._

import ducttape.versioner.WorkflowVersioner
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.workflow.HyperWorkflow

// workflow used for viz
class Executor(dirs: DirectoryArchitect,
               versions: WorkflowVersioner,
               packageVersioner: PackageVersioner,
               workflow: HyperWorkflow,
               plannedVertices: Set[(String,Realization)],
               alreadyDone: Set[(String,Realization)],
               todo: Set[(String,Realization)]) extends UnpackedDagVisitor {
  import ducttape.viz.WorkflowViz

  // TODO: Construct set elsewhere?
  val completed = new mutable.HashSet[(String,Realization)]
  val running = new mutable.HashSet[(String,Realization)]
  val failed = new mutable.HashSet[(String,Realization)]

  // TODO: Move all dot-related things to an instrumentation class
  dirs.xdotFile.synchronized {
    completed ++= alreadyDone
    Files.write(WorkflowViz.toGraphViz(workflow, plannedVertices, versions, completed, running, failed), dirs.xdotFile)
  }

  override def visit(task: RealTask) {
    if(todo((task.name, task.realization))) {
      val taskEnv = new FullTaskEnvironment(dirs, versions, packageVersioner, task)
      println("Running %s in %s".format(task.name, taskEnv.where.getAbsolutePath))

      dirs.xdotFile.synchronized {
        running += ((task.name, task.realization))
        Files.write(WorkflowViz.toGraphViz(workflow, plannedVertices, versions, completed, running, failed), dirs.xdotFile)
      }

      taskEnv.workDir.mkdirs
      if(!taskEnv.workDir.exists) {
        failed += ((task.name, task.realization))
        running -= ((task.name, task.realization))
        dirs.xdotFile.synchronized {
          Files.write(WorkflowViz.toGraphViz(workflow, plannedVertices, versions, completed, running, failed), dirs.xdotFile)
        }
        throw new RuntimeException("Could not make directory: " + taskEnv.where.getAbsolutePath)
      }

      // there's so many parameters here in case we want to "augment" the commands in some way
      val submitCommands = Submitter.prepare(dirs.workflowBaseDir, taskEnv.where, taskEnv.params,
                                             task.commands, task.name, task.realization, dirs.confBaseDir.getName)
      val exitCode = Shell.run(submitCommands, taskEnv.workDir, taskEnv.env, taskEnv.stdoutFile, taskEnv.stderrFile)
      Files.write("%d".format(exitCode), taskEnv.exitCodeFile)
      if(exitCode != 0) {
        // TODO: Send message via listener?
        println("Task %s/%s returned %s".format(task.name, task.realization.toString, exitCode))
        failed += ((task.name, task.realization))
        running -= ((task.name, task.realization))
        dirs.xdotFile.synchronized {
          Files.write(WorkflowViz.toGraphViz(workflow, plannedVertices, versions, completed, running, failed), dirs.xdotFile)
        }
        throw new RuntimeException("Task failed") // TODO: Catch and continue? Check for errors at end of visitor?
      }
    }
    completed += ((task.name, task.realization))
    running -= ((task.name, task.realization))
    dirs.xdotFile.synchronized {
      Files.write(WorkflowViz.toGraphViz(workflow, plannedVertices, versions, completed, running, failed), dirs.xdotFile)
    }
  }
}