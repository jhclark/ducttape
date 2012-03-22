package ducttape.exec

import collection._
import ducttape.Config
import ducttape.versioner.WorkflowVersioner
import ducttape.workflow.Types.HyperWorkflow
import ducttape.workflow.Realization
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.workflow.RealTask

// workflow used for viz
class Executor(conf: Config,
               dirs: DirectoryArchitect,
               versions: WorkflowVersioner,
               workflow: HyperWorkflow,
               plannedVertices: Set[(String,Realization)],
               alreadyDone: Set[(String,Realization)],
               todo: Set[(String,Realization)]) extends UnpackedDagVisitor {
  import ducttape.viz._

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
      val taskEnv = new TaskEnvironment(dirs, versions, task)
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
        println("%sTask %s/%s returned %s%s".format(conf.errorColor, task.name, task.realization.toString, exitCode, Console.RESET))
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