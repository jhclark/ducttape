// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import java.io.File

import collection._

import ducttape.util.Files
import ducttape.util.OrderedSet
import ducttape.util.MutableOrderedSet
import ducttape.workflow.Realization
import ducttape.workflow.VersionedTask
import ducttape.workflow.VersionedTaskId
import ducttape.versioner.WorkflowVersionInfo

import grizzled.slf4j.Logging

// checks the state of a task directory to make sure things completed as expected
// TODO: Return a set object with incomplete nodes that can be handed to future passes
// so that completion checking is atomic
object CompletionChecker extends Logging {
  
  def isExitCodeZero(exitCodeFile: File): Boolean = {
    try {
      Files.read(exitCodeFile)(0).trim == "0"
    } catch {
      case _: Throwable => false
    }
  } 
  
  def NO_CALLBACK(task: VersionedTask, msg: String) {}
  def isComplete(taskEnv: TaskEnvironment,
                 incompleteCallback: (VersionedTask, String) => Unit = NO_CALLBACK): Boolean = {
    
    // TODO: Grep stdout/stderr for "error"
    // TODO: Move this check and make it check file size and date with fallback to checksums? or always checksums? or checksum only if files are under a certain size?
    // use a series of thunks so that we don't try to open non-existent files
    val conditions: Seq[(() => Boolean, Option[String])] = (
      Seq(( () => taskEnv.where.exists, None), // no message, since this is normal
          ( () => taskEnv.exitCodeFile.exists, Some("Exit code file does not exist")),
          ( () => isExitCodeZero(taskEnv.exitCodeFile), Some("Non-zero exit code")),
          ( () => taskEnv.stdoutFile.exists, Some("Stdout file does not exist")),
          ( () => taskEnv.stderrFile.exists, Some("Stderr file does not exist")),
          ( () => !isInvalidated(taskEnv), Some("Previous version is complete, but invalidated"))) ++
      taskEnv.outputs.map { case (_, f: String) =>
        ( () => Files.exists(f), Some(s"${f} does not exist"))
      }
    )
    
    // check each condition necessary for a task to be complete
    // if a condition fails, notify the user why, if a message is provided
    conditions.forall { case (cond, msgOpt) =>
      val conditionHolds = cond()
      if (!conditionHolds) msgOpt match {
        case Some(msg) => incompleteCallback(taskEnv.task, msg)
        case None => ;
      }
      conditionHolds
    }
  }

  def isInvalidated(taskEnv: TaskEnvironment): Boolean = taskEnv.invalidatedFile.exists

  def invalidate(taskEnv: TaskEnvironment): Boolean = taskEnv.invalidatedFile.createNewFile

  // not recommended -- but sometimes manual intervention is required
  // and the user just wants the workflow to continue
  def forceCompletion(taskEnv: TaskEnvironment) {
    Files.write("0", taskEnv.exitCodeFile)
    val files = List(taskEnv.stdoutFile, taskEnv.stderrFile) ++ taskEnv.outputs.map {
      case (_,f) => new File(f)
    }
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
  def isLocked(taskEnv: TaskEnvironment) = taskEnv.lockFile.exists
}

// the initVersioner is generally the MostRecentWorkflowVersioner, so that we can check if
// the most recent result is untouched, invalid, partial, or complete
//
// use incompleteCallback to show info about why tasks aren't complete
class CompletionChecker(dirs: DirectoryArchitect,
                        unionVersion: WorkflowVersionInfo,
                        nextWorkflowVersion: Int,
                        incompleteCallback: (VersionedTask, String) => Unit)
    extends UnpackedDagVisitor with Logging {

  // we make a single pass to atomically determine what needs to be done
  // so that we can then prompt the user for confirmation
  // note: this is one of the major reasons that ducttape is a graph specification
  // language with an imperative language (bash) nested within --
  // if ducttape were turing complete, this multi-pass approach wouldn't be possible
  private val _completedVersions = new MutableOrderedSet[VersionedTaskId]
  private val _todoVersions = new MutableOrderedSet[VersionedTaskId]
  private val _completed = new MutableOrderedSet[(String,Realization)] // TODO: Change datatype of realization?
  private val _partial = new MutableOrderedSet[(String,Realization)] // not complete, but has partial output
  private val _todo = new MutableOrderedSet[(String,Realization)]
  private val _broken = new MutableOrderedSet[(String,Realization)]
  private val _locked = new MutableOrderedSet[(String,Realization)]

  // NOTE: completed never includes invalidated
  // TODO: Change these tuples to "RealTaskId"?
  def completedVersions: OrderedSet[VersionedTaskId] = _completedVersions
  def todoVersions: OrderedSet[VersionedTaskId] = _todoVersions
  def completed: OrderedSet[(String,Realization)] = _completed
  def partial: OrderedSet[(String,Realization)] = _partial
  def todo: OrderedSet[(String,Realization)] = _todo
  def broken: OrderedSet[(String,Realization)] = _broken
  def locked: OrderedSet[(String,Realization)] = _locked

  // the version of this task will be drawn from the "union" workflow version info
  override def visit(task: VersionedTask) {
    debug("Checking $task")
    val taskEnv = new TaskEnvironment(dirs, task)

    if (CompletionChecker.isComplete(taskEnv, incompleteCallback)) {
      _completedVersions += task.toVersionedTaskId
      _completed += ((task.name, task.realization))
    } else {
      // do NOT reuse the existing task for its version
      // since we're about to create a new version
      // and there's no way for the union workflow versioner
      // to know whether or not we'll use the existing version or not
      //
      // The walker is versioning with the "union" version --
      // If it has no version for this task, we'll get the "next" workflow version (which is fine)
      // But if it gave us a previous version, we want to reject that version and start a new one
      _todoVersions += new VersionedTaskId(task.namespace, task.realization.toCanonicalString(), nextWorkflowVersion)
      _todo += ((task.name, task.realization))
      debug(s"Todo: $task (Version $nextWorkflowVersion)")

      // Important: Check for locking *before* checking if something is broken
      // since not all output files may exist while another process is working on this task
      if (CompletionChecker.isLocked(taskEnv)) {
        debug(s"Locked: $task")
        _locked += ((task.name, task.realization))
        
      } else if (CompletionChecker.isBroken(taskEnv)) {
        debug(s"Broken: $task")
        _broken += ((task.name, task.realization))
        
      } else if (CompletionChecker.hasPartialOutput(taskEnv)) {
        debug(s"Partially complete: $task")
        _partial += ((task.name, task.realization))
      }
    }
  }
}
