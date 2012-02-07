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
      Seq(( () => isInvalidated(taskEnv), "Previous version is complete, but invalidated"))

    )
    for( (cond, msg) <- conditions; if(!cond())) {
      System.err.println("Task incomplete %s/%s: %s".format(taskEnv.task.name, taskEnv.task.realizationName, msg))
      return false
    }
    return true
  }

  def isInvalidated(taskEnv: TaskEnvironment): Boolean = taskEnv.invalidatedFile.exists

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
class CompletionChecker(conf: Config, dirs: DirectoryArchitect, initVersioner: WorkflowVersioner) extends UnpackedDagVisitor {
  // we make a single pass to atomically determine what needs to be done
  // so that we can then prompt the user for confirmation
  private val complete = new OrderedSet[(String,String)] // TODO: Change datatype of realization?
  private val partialOutput = new OrderedSet[(String,String)] // not complete, but has partial output
  private val invalid = new OrderedSet[(String,String)] // invalidated by user (whether complete or not)
  private val todoList = new OrderedSet[(String,String)]

  // what is the workflow version of the completed version that we'll be reusing?
  private val foundVersions = new mutable.HashMap[(String,String), Int]

  // return immutable views:
  def completed: Set[(String,String)] = complete
  def partial: Set[(String,String)] = partialOutput
  def invalidated: Set[(String,String)] = invalid
  def todo: Set[(String,String)] = todoList

  // the workflow versions of each completed unpacked task
  def completedVersions: Map[(String,String),Int] = foundVersions

  override def visit(task: RealTask) {
    val taskEnv = new TaskEnvironment(dirs, initVersioner, task)
    if(CompletionChecker.isComplete(taskEnv)) {
      complete += ((task.name, task.realizationName))
    } else {
      todoList += ((task.name, task.realizationName))
      if(PartialOutputRemover.hasPartialOutput(taskEnv)) {
        partialOutput += ((task.name, task.realizationName))
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
                           partial: Set[(String,String)]) extends UnpackedDagVisitor {
  override def visit(task: RealTask) {
    val taskEnv = new TaskEnvironment(dirs, versions, task)
    if(partial( (task.name, task.realizationName) )) {
      err.println("Removing partial output at %s".format(taskEnv.where))
      Files.deleteDir(taskEnv.where)
    }
  }  
}

class Builder(conf: Config,
              dirs: DirectoryArchitect,
              versions: WorkflowVersioner,
              todo: Set[(String,String)]) extends UnpackedDagVisitor {

  override def visit(task: RealTask) {
    val taskEnv = new TaskEnvironment(dirs, versions, task)
    if(todo( (task.name, task.realizationName) )) {
      println("%sBuilding tools for: %s/%s%s".format(conf.taskColor, task.name, task.realizationName, Console.RESET))
      // TODO: Rename the augment method
      taskEnv.workDir.mkdirs
      val gimmeCmds = Gimme.augment(dirs.baseDir, taskEnv.params, Seq.empty)
      val exitCode = Shell.run(gimmeCmds, taskEnv.workDir, taskEnv.env, taskEnv.buildStdoutFile, taskEnv.buildStderrFile)
      if(exitCode != 0) {
        println("%sBuild task %s/%s returned %s%s".format(conf.errorColor, task.name, task.realizationName, exitCode, Console.RESET))
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
               alreadyDone: Set[(String,String)],
               todo: Set[(String,String)]) extends UnpackedDagVisitor {
  import ducttape.viz._

  // TODO: Construct set elsewhere?
  val completed = new mutable.HashSet[(String,String)]
  val running = new mutable.HashSet[(String,String)]
  val failed = new mutable.HashSet[(String,String)]
  dirs.xdotFile.synchronized {
    completed ++= alreadyDone
    Files.write(WorkflowViz.toGraphViz(workflow, versions, completed, running, failed), dirs.xdotFile)
  }

  override def visit(task: RealTask) {
    if(todo((task.name, task.realizationName))) {
      val taskEnv = new TaskEnvironment(dirs, versions, task)
      println("Running %s in %s".format(task.name, taskEnv.where.getAbsolutePath))

      dirs.xdotFile.synchronized {
        running += ((task.name, task.realizationName))
        Files.write(WorkflowViz.toGraphViz(workflow, versions, completed, running, failed), dirs.xdotFile)
      }

      taskEnv.workDir.mkdirs
      if(!taskEnv.workDir.exists) {
        failed += ((task.name, task.realizationName))
        running -= ((task.name, task.realizationName))
        dirs.xdotFile.synchronized {
          Files.write(WorkflowViz.toGraphViz(workflow, versions, completed, running, failed), dirs.xdotFile)
        }
        throw new RuntimeException("Could not make directory: " + taskEnv.where.getAbsolutePath)
      }
      
      // there's so many parameters here in case we want to "augment" the commands in some way
      val submitCommands = Submitter.prepare(dirs.baseDir, taskEnv.where, taskEnv.params,
                                             task.commands, task.name, task.realizationName)
      val exitCode = Shell.run(submitCommands, taskEnv.workDir, taskEnv.env, taskEnv.stdoutFile, taskEnv.stderrFile)
      Files.write("%d".format(exitCode), taskEnv.exitCodeFile)
      if(exitCode != 0) {
        println("%sTask %s/%s returned %s%s".format(conf.errorColor, task.name, task.realizationName, exitCode, Console.RESET))
        failed += ((task.name, task.realizationName))
        running -= ((task.name, task.realizationName))
        dirs.xdotFile.synchronized {
          Files.write(WorkflowViz.toGraphViz(workflow, versions, completed, running, failed), dirs.xdotFile)
        }
        throw new RuntimeException("Task failed") // TODO: Catch and continue? Check for errors at end of visitor?
      }
    }
    completed += ((task.name, task.realizationName))
    running -= ((task.name, task.realizationName))
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
