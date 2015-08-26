// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import sys.ShutdownHookThread

import ducttape.util.Files
import ducttape.util.Optional
import ducttape.versioner.WorkflowVersionStore

import java.io.File
import java.nio.channels.FileLock
import java.io.RandomAccessFile

import grizzled.slf4j.Logging

import collection._

object LockManager extends Logging {
  // take a thunk so that we can create a scoping effect
  def apply[U](workflowVersion: WorkflowVersionStore)(func: LockManager => U) {
    // note: LockManager internally starts a JVM shutdown hook to release locks on JVM shutdown
    val locker = new LockManager(workflowVersion)
    func(locker)
    locker.shutdown()
  }

  // DO NOT use this during normal workflow execution
  // it is meant for manual one-time cleanup only
  def forceReleaseLock(taskEnv: TaskEnvironment) {
    debug("Forcefully releasing lock: " + taskEnv.lockFile)
 
/*   
    locks.synchronized {
      locks.getOrElse(taskEnv.lockFile.getAbsolutePath) match {
        case Some(lock) => {
          lock.release()
          locks -= taskEnv.lockFile.getAbsolutePath          
        }
        case None => ;
      }
    }
*/
    taskEnv.lockFile.delete()
  }
}


// see also PidWriter
// note: this class is also responsible for writing workflow version information
class LockManager(version: WorkflowVersionStore) extends ExecutionObserver with Logging {
  
  // key is absolute path to file
  val locks = new mutable.HashMap[String, FileLock] with mutable.SynchronizedMap[String, FileLock]
  private var isShutdown = false

  private val locker = this
  private val unlocker = ShutdownHookThread {
    // release all of our locks, even if we go down in flames
    // note: 'finally' blocks aren't guaranteed to be executed on JVM shutdown and we really need these files removed...
    locker.shutdown()
    if (locks.size > 0) {
      System.err.println("EXITING: Cleaning up lock files...")
      // don't accept any more locks for the lifefime of this object once we detect that a JVM shutdown is in progress
      locker.maybeReleaseAll()
    }
  }

  override def skip(exec: Executor, taskEnv: TaskEnvironment) {}
  
  override def begin(exec: Executor, taskEnv: FullTaskEnvironment) = acquireLock(taskEnv)
  override def fail(exec: Executor, taskEnv: FullTaskEnvironment)  = releaseLock(taskEnv)
  override def succeed(exec: Executor, taskEnv: FullTaskEnvironment) = releaseLock(taskEnv)
  
  // returns hostname and pid
  def readLockFile(file: File): (String, Int) = {
    try {
      Files.read(file) match {
        case Seq(line) => {
          val Array(hostname, pid) = line.split(":")
          (hostname, pid.toInt)
        }
        // throw MatchError if file is malformed
      }
    } catch {
      case _: Throwable => throw new RuntimeException("Corrupt lock file: " + file.getAbsolutePath)
    }
  }
  
  /**
   * Usually, we already acquired the lock, long before the task actually started running,
   * when the workflow began running. However, sometimes we must wait for another ducttape
   * process to either complete a task or fail to complete it before we can acquire a lock
   */
  def acquireLock(taskEnv: TaskEnvironment) {

    if (isShutdown) {
      throw new RuntimeException("LockManager is already shut down. No further locks will be issued.")
    }
    
    // do we already hold this lock?
    debug("Checking if we need to lock: %s".format(taskEnv.lockFile.getAbsolutePath))
    if (!locks.contains(taskEnv.lockFile.getAbsolutePath)) {
      // try to figure out (in a non-strict way) whether we will likely
      // need to wait on the lock and give the user some feedback
      // if we do
      if (taskEnv.lockFile.exists) {
        try {
          val (hostname, pid) = readLockFile(taskEnv.lockFile)
          debug("Waiting for lock held by %s:%d: %s".format(hostname, pid, taskEnv.lockFile))
        } catch {
          case _: Throwable => ; // something went wrong, but just ignore it since this was only informational
        }
      }
        
      Files.mkdirs(taskEnv.lockFile.getParentFile)
      val raFile = new RandomAccessFile(taskEnv.lockFile, "rws")
    
      debug("Waiting for lock: %s".format(taskEnv.lockFile))

      taskEnv.lockFile.synchronized {
        // use the OS's file locking mechanism
        // NOTE: This is valid only on the same machine -- not across network filesystems such as NFS!
        // this will block until our JVM acquires the lock
        val lock = raFile.getChannel.lock()
        
        // remember: we already have "locks" synchronized
        locks += taskEnv.lockFile.getAbsolutePath -> lock
        
        // XXX: Usually locks are used with a try-finally block
        // can we use some callback function here to make this a bit cleaner?
        
        debug("Writing lock: " + taskEnv.lockFile)
        Files.write("%s:%d".format(version.hostname, version.pid), taskEnv.lockFile)
        debug("Sucessfully acquired lock: " + taskEnv.lockFile)
    
        // did this task already get completed while we were waiting?
        if (!CompletionChecker.isComplete(taskEnv)) {
          // We need to cleanup if someone else previously held this lock
          // (specifically, we need the workflow version to match our version)
          if (taskEnv.versionFile.exists) {
            val versionMatches: Boolean = {
              try {
                val oldVersion = Files.read(taskEnv.versionFile).head.toInt
                oldVersion == version.version
              } catch {
                case _: Throwable => false
              }
            }
    
            // move old output to attic (if any)
            if (!versionMatches) {
              PartialOutputMover.moveToAttic(taskEnv)
            }
          }
        }
    
        // write the version file *after* removing any previous partial output
        Files.write(version.version.toString, taskEnv.versionFile) 
      }
    }
  }

  def releaseLock(taskEnv: TaskEnvironment) {
    debug("Releasing lock: " + taskEnv.lockFile)
    
    taskEnv.lockFile.synchronized {
      val lock = locks(taskEnv.lockFile.getAbsolutePath)
      taskEnv.lockFile.delete()
      locks -= taskEnv.lockFile.getAbsolutePath
      lock.release()
    }
  }

  // acquire the lock iff nobody else holds it
  def maybeAcquireLock(taskEnv: TaskEnvironment, writeVersion: Boolean): Boolean = {

    if (isShutdown) {
      false
    } else {

      // do we already hold this lock?
      debug("Checking if we need to lock: %s".format(taskEnv.lockFile.getAbsolutePath))
      if (!locks.contains(taskEnv.lockFile.getAbsolutePath)) {
        
        Files.mkdirs(taskEnv.lockFile.getParentFile)
        val raFile = new RandomAccessFile(taskEnv.lockFile, "rws")
        Optional.toOption(raFile.getChannel.tryLock()) match {
          case Some(lock) => {
            debug("Successfully got an early lock on: " + taskEnv.lockFile)
            // note: we already have "locks" synchronized
            locks += taskEnv.lockFile.getAbsolutePath -> lock
            debug("Locks are now: " + locks)
            if (writeVersion) {
              debug("Writing version to " + taskEnv.versionFile)
              Files.write(version.version.toString, taskEnv.versionFile)
            }
            true
          }
          case None => {
            debug("Did not get an early lock on: " + taskEnv.lockFile)
              false
          }
        }
      } else {
        debug("Already locked: %s".format(taskEnv.lockFile.getAbsolutePath))
        true
      }
    }
  }

  def maybeReleaseAll() = locks.keys.foreach { absPath => maybeReleaseLock(absPath) }

  private def maybeReleaseLock(absPath: String): Boolean = {
    locks.get(absPath) match {
      case None => {
        debug("No need to release lock (it's not ours): " + absPath)
        false
      }
      case Some(lock) => {
        debug("Releasing lock (as part of cleanup): " + absPath)
        new File(absPath).delete()
        locks -= absPath
        lock.release()
        true
      }
    }
  }

  // release the lock iff we hold it
  def maybeReleaseLock(taskEnv: TaskEnvironment): Boolean = maybeReleaseLock(taskEnv.lockFile.getAbsolutePath)

  // used to indicate that this lock manager should not accept any further requests
  // to acquire locks
  // this can be useful when a user shutdown request has been detected (e.g. Ctrl+C)
  // but not all worker threads have stopped yet. we need to guarantee that no new locks
  // will be issued during cleanup
  def shutdown() {
    this.synchronized {
      isShutdown = true
    }
  }
}
