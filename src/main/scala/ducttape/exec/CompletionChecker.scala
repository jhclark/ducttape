package ducttape.exec

import java.io.File

import collection._

import ducttape.Config
import ducttape.workflow.Realization
import ducttape.util.Files
import ducttape.versioner.WorkflowVersioner
import ducttape.workflow.RealTask

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
          ( () => try { Files.read(taskEnv.exitCodeFile)(0).trim == "0" } catch { case _ => false }, "Non-zero exit code"),
          ( () => taskEnv.stdoutFile.exists, "Stdout file does not exist"),
          ( () => taskEnv.stderrFile.exists, "Stderr file does not exist")) ++
      taskEnv.outputs.map{case (_,f) => ( () => new File(f).exists, "%s does not exist".format(f))} ++
      Seq(( () => !isInvalidated(taskEnv), "Previous version is complete, but invalidated"))

    )
    for( (cond, msg) <- conditions) {
      if(!cond()) {
        System.err.println("Task incomplete %s/%s: %s".format(taskEnv.task.name, taskEnv.task.realization.toString, msg))
        return false
      }
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
                        initVersioner: WorkflowVersioner) extends UnpackedDagVisitor {
  // we make a single pass to atomically determine what needs to be done
  // so that we can then prompt the user for confirmation
  import ducttape.ccollection._
  private val complete = new MutableOrderedSet[(String,Realization)] // TODO: Change datatype of realization?
  private val partialOutput = new MutableOrderedSet[(String,Realization)] // not complete, but has partial output
  private val invalid = new MutableOrderedSet[(String,Realization)] // invalidated by user (whether complete or not)
  private val todoList = new MutableOrderedSet[(String,Realization)]

  // what is the workflow version of the completed version that we'll be reusing?
  private val _foundVersions = new mutable.HashMap[(String,Realization), Int]
  private val completeVersions = new mutable.HashMap[(String,Realization), Int]

  // TODO: return immutable views:
  def completed: OrderedSet[(String,Realization)] = complete
  def partial: OrderedSet[(String,Realization)] = partialOutput
  def invalidated: OrderedSet[(String,Realization)] = invalid
  def todo: OrderedSet[(String,Realization)] = todoList

  // the workflow versions of each completed unpacked task
  def completedVersions: Map[(String,Realization),Int] = completeVersions
  def foundVersions: Map[(String,Realization),Int] = _foundVersions

  override def visit(task: RealTask) {
    //System.err.println("Checking " + task)
    val taskEnv = new TaskEnvironment(dirs, initVersioner, task)
    if(taskEnv.where.exists) {
      _foundVersions += (task.name, task.realization) -> task.version
    }
    if(CompletionChecker.isComplete(taskEnv)) {
      complete += ((task.name, task.realization))
      completeVersions += (task.name, task.realization) -> task.version
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
