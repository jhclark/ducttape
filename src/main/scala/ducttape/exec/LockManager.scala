package ducttape.exec

import ducttape.workflow.RealTask
import ducttape.util.Files
import ducttape.util.Optional
import ducttape.versioner.WorkflowVersionInfo

import java.io.File
import java.nio.channels.FileLock
import java.io.RandomAccessFile

import grizzled.slf4j.Logging

import collection._

// see also PidWriter
class LockManager(version: WorkflowVersionInfo) extends ExecutionObserver with Logging {
  
  // key is absolute path to file
  val locks = new mutable.HashMap[String, FileLock]
  
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
      case _ => throw new RuntimeException("Corrupt lock file: " + file.getAbsolutePath)
    }
  }
  
  /**
   * Usually, we already acquired the lock, long before the task actually started running,
   * when the workflow began running. However, sometimes we must wait for another ducttape
   * process to either complete a task or fail to complete it before we can acquire a lock
   */
  def acquireLock(taskEnv: TaskEnvironment) {
    
    // try to figure out (in a non-strict way) whether we will likely
    // need to wait on the lock and give the user some feedback
    // if we do
    if (taskEnv.lockFile.exists) {
      val (hostname, pid) = readLockFile(taskEnv.lockFile)
      debug("Waiting for lock held by %s:%d: %s".format(hostname, pid, taskEnv.lockFile))
    }
    
    val raFile = new RandomAccessFile(taskEnv.lockFile, "rws")

    // TODO: How do we decide whether to remove or skip?

    // use the OS's file locking mechanism
    // NOTE: This is valid only on the same machine -- not across network filesystems such as NFS!
    // this will block until our JVM acquires the lock
    val lock = raFile.getChannel.lock()
    locks.synchronized {
      locks += taskEnv.lockFile.getAbsolutePath -> lock
    }
    
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
            case _ => false
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

  def releaseLock(taskEnv: TaskEnvironment) {
    debug("Releasing lock: " + taskEnv.lockFile)
    
    locks.synchronized {
      val lock = locks(taskEnv.lockFile.getAbsolutePath)
      taskEnv.lockFile.delete()
      locks -= taskEnv.lockFile.getAbsolutePath
      lock.release()
    }
  }

  // acquire the lock iff nobody else holds it
  def maybeAcquireLock(taskEnv: TaskEnvironment) {
    val raFile = new RandomAccessFile(taskEnv.lockFile, "rws")
    Optional.toOption(raFile.getChannel.tryLock()) match {
      case Some(lock) => {
        debug("Successfully got an early lock on: " + taskEnv.lockFile)
        locks.synchronized {
          locks += taskEnv.lockFile.getAbsolutePath -> lock
        }
      }
      case None => debug("Did not get an early lock on: " + taskEnv.lockFile)
    }
  }

  // release the lock iff we hold it
  def maybeReleaseLock(taskEnv: TaskEnvironment) {
    locks.synchronized {
      locks.get(taskEnv.lockFile.getAbsolutePath) match {
        case None => debug("No need to release lock (it's not ours): " + taskEnv.lockFile)
        case Some(lock) => {
          debug("Releasing lock (as part of cleanup): " + taskEnv.lockFile)
          taskEnv.lockFile.delete()
          locks -= taskEnv.lockFile.getAbsolutePath
          lock.release()
        }
      }
    }
  }
}
