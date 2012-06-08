package ducttape.exec

import java.io.File
import collection._
import ducttape.workflow.Realization
import ducttape.util.Files
import ducttape.util.OrderedSet
import ducttape.util.MutableOrderedSet
import ducttape.workflow.RealTask
import ducttape.versioner.TaskVersion

import grizzled.slf4j.Logging

/**
 * Moves incomplete or invalidated tasks to the attic before re-running them.
 * The CompletionChecker has already discovered which tasks are broken or partially completed.
 * 
 * partial is a list of the task/realizations that have partial output that needs to be moved
 *         (and are guaranteed to have version numbers)
 * broken is a list of the task/realizations that are partial but are missing information
 *        such as a version number and so are considered broken. these will be deleted
 */
class PartialOutputMover(dirs: DirectoryArchitect,
                         partial: Set[(String,Realization)],
                         broken: Set[(String,Realization)],
                         locker: LockManager) extends UnpackedDagVisitor with Logging {
  
  override def visit(task: RealTask) {
    
    debug("Considering %s".format(task))
    
    val taskEnv = new TaskEnvironment(dirs, task)
    val gotLock = locker.maybeAcquireLock(taskEnv)
    
    if (gotLock) {
      if (broken( (task.name, task.realization) )) {
        System.err.println("Removing broken partial output for %s".format(task))
        val origDir = dirs.assignDir(task)
        Files.deleteDir(origDir)
        
      } else if (partial( (task.name, task.realization) )) {
        System.err.println("Moving %s to the attic".format(task))
        PartialOutputMover.moveToAttic(taskEnv)
      }
    } else {
      debug("Couldn't immediately get a lock for %s. We'll let the other process keep using it for now and try again later".format(task))
    }
  }
}

object PartialOutputMover {
  def moveToAttic(taskEnv: TaskEnvironment) {
    val task = taskEnv.task

    val version = TaskVersion.read(taskEnv.versionFile)
    val origDir = taskEnv.dirs.assignDir(task)
    val atticDir = taskEnv.dirs.assignAtticDir(task, version)
    Files.moveDir(origDir, atticDir)
  }
}
