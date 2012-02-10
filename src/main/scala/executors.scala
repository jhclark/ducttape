package ducttape

import System._
import collection._

import java.io.File

import ducttape.hyperdag._
import ducttape.environment._
import ducttape.Types._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.workflow._
import ducttape.util._
import ducttape.versioner._

// TODO: Abstract beyond just workflows?
trait PackedDagVisitor {
  def visit(task: TaskTemplate)
}

trait UnpackedDagVisitor {
  def visit(task: RealTask)
}

// checks the state of a task directory to make sure things completed as expected
// TODO: Return a set object with incomplete nodes that can be handed to future passes
// so that completion checking is atomic
object CompletionChecker {
  def isComplete(taskEnv: TaskEnvironment): Boolean = {
    // TODO: Grep stdout/stderr for "error"
    // TODO: Move this check and make it check file size and date with fallback to checksums? or always checksums? or checksum only if files are under a certain size?
    // use a series of thunks so that we don't try to open non-existent files
    val conditions: Seq[(() => Boolean, String)] = (
      Seq(( () => taskEnv.where.exists, "No previous output"),
          ( () => taskEnv.exitCodeFile.exists, "Exit code file does not exist"),
          ( () => io.Source.fromFile(taskEnv.exitCodeFile).getLines.next.trim == "0", "Non-zero exit code"),
          ( () => taskEnv.stdoutFile.exists, "Stdout file does not exist"),
          ( () => taskEnv.stderrFile.exists, "Stderr file does not exist")) ++
      taskEnv.outputs.map{case (_,f) => ( () => new File(f).exists, "%s does not exist")} ++
      Seq(( () => !isInvalidated(taskEnv), "Previous version is complete, but invalidated"))

    )
    for( (cond, msg) <- conditions; if(!cond())) {
      System.err.println("Task incomplete %s/%s: %s".format(taskEnv.task.name, taskEnv.task.realization.toString, msg))
      return false
    }
    return true
  }

  def isInvalidated(taskEnv: TaskEnvironment): Boolean = taskEnv.invalidatedFile.exists

  def invalidate(taskEnv: TaskEnvironment): Boolean = taskEnv.invalidatedFile.createNewFile

  // not recommended -- but sometimes manual intervention is required
  // and the user just wants the workflow to continue
  def forceCompletion(taskEnv: TaskEnvironment) {
    Files.write("0", taskEnv.exitCodeFile)
    val files = List(taskEnv.stdoutFile, taskEnv.stderrFile) ++ taskEnv.outputs.map{case (_,f) => new File(f)}
    for(file <- files) {
      if(!taskEnv.stdoutFile.exists) {
        Files.write("", taskEnv.stdoutFile)
      }
    }
    if(!isComplete(taskEnv)) {
      throw new RuntimeException("Failed to force completion of task")
    }
  }
}

// the initVersioner is generally the MostRecentWorkflowVersioner, so that we can check if
// the most recent result is untouched, invalid, partial, or complete
class CompletionChecker(conf: Config,
                        dirs: DirectoryArchitect,
                        initVersioner: WorkflowVersioner,
                        planned: Set[(String,Realization)]) extends UnpackedDagVisitor {
  // we make a single pass to atomically determine what needs to be done
  // so that we can then prompt the user for confirmation
  import ducttape.ccollection._
  private val complete = new MutableOrderedSet[(String,Realization)] // TODO: Change datatype of realization?
  private val partialOutput = new MutableOrderedSet[(String,Realization)] // not complete, but has partial output
  private val invalid = new MutableOrderedSet[(String,Realization)] // invalidated by user (whether complete or not)
  private val todoList = new MutableOrderedSet[(String,Realization)]

  // what is the workflow version of the completed version that we'll be reusing?
  private val foundVersions = new mutable.HashMap[(String,Realization), Int]

  // TODO: return immutable views:
  def completed: OrderedSet[(String,Realization)] = complete
  def partial: OrderedSet[(String,Realization)] = partialOutput
  def invalidated: OrderedSet[(String,Realization)] = invalid
  def todo: OrderedSet[(String,Realization)] = todoList

  // the workflow versions of each completed unpacked task
  def completedVersions: Map[(String,Realization),Int] = foundVersions

  override def visit(task: RealTask) {
    if(planned( (task.name, task.realization) )) {
      val taskEnv = new TaskEnvironment(dirs, initVersioner, task)
      if(CompletionChecker.isComplete(taskEnv)) {
        complete += ((task.name, task.realization))
        foundVersions += (task.name, task.realization) -> task.version
      } else {
        todoList += ((task.name, task.realization))
        if(CompletionChecker.isInvalidated(taskEnv)) {
          invalid += ((task.name, task.realization))
        } else {
          if(PartialOutputRemover.hasPartialOutput(taskEnv)) {
            partialOutput += ((task.name, task.realization))
          }
        }
      }
    }
  }
}

object PartialOutputRemover {
  def hasPartialOutput(taskEnv: TaskEnvironment) = taskEnv.where.exists
}

class PartialOutputRemover(conf: Config,
                           dirs: DirectoryArchitect,
                           versions: WorkflowVersioner,
                           partial: Set[(String,Realization)]) extends UnpackedDagVisitor {
  override def visit(task: RealTask) {
    val taskEnv = new TaskEnvironment(dirs, versions, task)
    if(partial( (task.name, task.realization) )) {
      err.println("Removing partial output at %s".format(taskEnv.where))
      Files.deleteDir(taskEnv.where)
    }
  }  
}

// dirs and versions are unimportant other than being required to generate the TaskEnvironment
class PackageFinder(conf: Config,
                    dirs: DirectoryArchitect,
                    versions: WorkflowVersioner,
                    todo: Set[(String,Realization)]) extends UnpackedDagVisitor {
  val packages = new mutable.HashSet[String]
  override def visit(task: RealTask) {
    if(todo((task.name, task.realization))) {
      val taskEnv = new TaskEnvironment(dirs, versions, task)
      packages ++= taskEnv.packageNames
    }
  }
}

class PackageBuilder(conf: Config, dirs: DirectoryArchitect, workflowVersion: Int) {
  def build(packageNames: Iterable[String]) {
    for(packageName <- packageNames) {
      val buildEnv = new BuildEnvironment(dirs, workflowVersion, packageName)
      println("%sBuilding tools for: %s in %s%s".format(conf.taskColor, packageName, buildEnv.buildDir, Console.RESET))
      // TODO: XXX: Can build ever interfere with another running workflow?
      println("Removing: %s".format(buildEnv.buildDir.toString))
      if(buildEnv.buildDir.exists) {
        Files.deleteDir(buildEnv.buildDir)
      }
      buildEnv.buildDir.mkdirs

      val gimmeCmds = Seq(Gimme.getCommand(dirs.baseDir, packageName))
      val env = Seq()
      val exitCode = Shell.run(gimmeCmds, buildEnv.buildDir, env, buildEnv.buildStdoutFile, buildEnv.buildStderrFile)
      if(exitCode != 0) {
        println("%sBuild task %s returned %s%s".format(conf.errorColor, packageName, exitCode, Console.RESET))
        exit(1)
      }

    }
  }
}

// workflow used for viz
class Executor(conf: Config,
               dirs: DirectoryArchitect,
               versions: WorkflowVersioner,
               workflow: HyperWorkflow,
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
    Files.write(WorkflowViz.toGraphViz(workflow, versions, completed, running, failed), dirs.xdotFile)
  }

  override def visit(task: RealTask) {
    if(todo((task.name, task.realization))) {
      val taskEnv = new TaskEnvironment(dirs, versions, task)
      println("Running %s in %s".format(task.name, taskEnv.where.getAbsolutePath))

      dirs.xdotFile.synchronized {
        running += ((task.name, task.realization))
        Files.write(WorkflowViz.toGraphViz(workflow, versions, completed, running, failed), dirs.xdotFile)
      }

      taskEnv.workDir.mkdirs
      if(!taskEnv.workDir.exists) {
        failed += ((task.name, task.realization))
        running -= ((task.name, task.realization))
        dirs.xdotFile.synchronized {
          Files.write(WorkflowViz.toGraphViz(workflow, versions, completed, running, failed), dirs.xdotFile)
        }
        throw new RuntimeException("Could not make directory: " + taskEnv.where.getAbsolutePath)
      }

      // there's so many parameters here in case we want to "augment" the commands in some way
      val submitCommands = Submitter.prepare(dirs.baseDir, taskEnv.where, taskEnv.params,
                                             task.commands, task.name, task.realization)
      val exitCode = Shell.run(submitCommands, taskEnv.workDir, taskEnv.env, taskEnv.stdoutFile, taskEnv.stderrFile)
      Files.write("%d".format(exitCode), taskEnv.exitCodeFile)
      if(exitCode != 0) {
        println("%sTask %s/%s returned %s%s".format(conf.errorColor, task.name, task.realization.toString, exitCode, Console.RESET))
        failed += ((task.name, task.realization))
        running -= ((task.name, task.realization))
        dirs.xdotFile.synchronized {
          Files.write(WorkflowViz.toGraphViz(workflow, versions, completed, running, failed), dirs.xdotFile)
        }
        throw new RuntimeException("Task failed") // TODO: Catch and continue? Check for errors at end of visitor?
      }
    }
    completed += ((task.name, task.realization))
    running -= ((task.name, task.realization))
    dirs.xdotFile.synchronized {
      Files.write(WorkflowViz.toGraphViz(workflow, versions, completed, running, failed), dirs.xdotFile)
    }
  }
}

class Purger(conf: Config, dirs: DirectoryArchitect) extends PackedDagVisitor {
  override def visit(task: TaskTemplate) {
    val where = dirs.assignPackedDir(task.taskDef.name)
    println("Removing directory: %s".format(where.getAbsolutePath))
    if(where.exists) {
      Files.deleteDir(where)
    }
  }
}
