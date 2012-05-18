package ducttape.exec

import java.io.File

import collection._

import ducttape.workflow.Realization
import ducttape.util.Files
import ducttape.util.OrderedSet
import ducttape.util.MutableOrderedSet
import ducttape.workflow.RealTask
import grizzled.slf4j.Logging

// checks the state of a task directory to make sure things completed as expected
// TODO: Return a set object with incomplete nodes that can be handed to future passes
// so that completion checking is atomic
object CompletionChecker extends Logging {
  def isComplete(taskEnv: TaskEnvironment): Boolean = {
    // TODO: Grep stdout/stderr for "error"
    // TODO: Move this check and make it check file size and date with fallback to checksums? or always checksums? or checksum only if files are under a certain size?
    // use a series of thunks so that we don't try to open non-existent files
    val conditions: Seq[(() => Boolean, String)] = (
      Seq(( () => taskEnv.where.exists, "No previous output"),
          ( () => taskEnv.exitCodeFile.exists, "Exit code file does not exist"),
          ( () => try { Files.read(taskEnv.exitCodeFile)(0).trim == "0" } catch { case _ => false }, "Non-zero exit code"),
          ( () => taskEnv.stdoutFile.exists, "Stdout file does not exist"),
          ( () => taskEnv.stderrFile.exists, "Stderr file does not exist"),
          ( () => !isInvalidated(taskEnv), "Previous version is complete, but invalidated")) ++
          
      taskEnv.outputs.map { case (_,f) => ( () => new File(f).exists, "%s does not exist".format(f)) }
    )
    
    conditions.forall { case (cond, msg) =>
      val conditionHolds = cond()
      if (!conditionHolds)
        System.err.println("Task incomplete %s/%s: %s".format(taskEnv.task.name, taskEnv.task.realization.toString, msg))
      conditionHolds
    }
  }

  def isInvalidated(taskEnv: TaskEnvironment): Boolean = taskEnv.invalidatedFile.exists

  def invalidate(taskEnv: TaskEnvironment): Boolean = taskEnv.invalidatedFile.createNewFile

  // not recommended -- but sometimes manual intervention is required
  // and the user just wants the workflow to continue
  def forceCompletion(taskEnv: TaskEnvironment) {
    Files.write("0", taskEnv.exitCodeFile)
    val files = List(taskEnv.stdoutFile, taskEnv.stderrFile) ++ taskEnv.outputs.map{case (_,f) => new File(f)}
    for (file <- files) {
      if (!taskEnv.stdoutFile.exists) {
        Files.write("", taskEnv.stdoutFile)
      }
    }
    if (!isComplete(taskEnv)) {
      throw new RuntimeException("Failed to force completion of task")
    }
  }
  
  def hasPartialOutput(taskEnv: TaskEnvironment) = taskEnv.where.exists
  def isBroken(taskEnv: TaskEnvironment) = taskEnv.where.exists && !taskEnv.versionFile.exists
}

// the initVersioner is generally the MostRecentWorkflowVersioner, so that we can check if
// the most recent result is untouched, invalid, partial, or complete
class CompletionChecker(dirs: DirectoryArchitect) extends UnpackedDagVisitor with Logging {
  // we make a single pass to atomically determine what needs to be done
  // so that we can then prompt the user for confirmation
  private val _completed = new MutableOrderedSet[(String,Realization)] // TODO: Change datatype of realization?
  private val _partial = new MutableOrderedSet[(String,Realization)] // not complete, but has partial output
  private val _todo = new MutableOrderedSet[(String,Realization)]
  private val _broken = new MutableOrderedSet[(String,Realization)]

  // what is the workflow version of the completed version that we'll be reusing?
  private val _foundVersions = new mutable.HashMap[(String,Realization), Int]
  private val completeVersions = new mutable.HashMap[(String,Realization), Int]

  // NOTE: completed never includes invalidated
  def completed: OrderedSet[(String,Realization)] = _completed
  def partial: OrderedSet[(String,Realization)] = _partial
  def todo: OrderedSet[(String,Realization)] = _todo
  def broken: OrderedSet[(String,Realization)] = _broken

  override def visit(task: RealTask) {
    debug("Checking " + task)
    val taskEnv = new TaskEnvironment(dirs, task)

    if (CompletionChecker.isComplete(taskEnv)) {
      _completed += ((task.name, task.realization))
//      completeVersions += (task.name, task.realization) -> task.version
    } else {
      _todo += ((task.name, task.realization))
      
      if (CompletionChecker.isBroken(taskEnv)) {
        _broken += ((task.name, task.realization))
      } else if (CompletionChecker.hasPartialOutput(taskEnv)) {
        _partial += ((task.name, task.realization))
      }
    }
  }
}
