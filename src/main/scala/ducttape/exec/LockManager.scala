package ducttape.exec

import ducttape.workflow.RealTask
import ducttape.util.Files
import ducttape.versioner.WorkflowVersionInfo
import scala.annotation.tailrec
import java.io.File
import grizzled.slf4j.Logging

// see also PidWriter
class LockManager(version: WorkflowVersionInfo) extends ExecutionObserver with Logging {
  
  val LOCK_POLLING_TIMEOUT_SECS = 30

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
  @tailrec final def acquireLock(taskEnv: TaskEnvironment) {
    // check if lock belongs to us
    // if not, wait for it and then claim it
    if (taskEnv.lockFile.exists) {
      val (hostname, pid) = readLockFile(taskEnv.lockFile)
      if (hostname == version.hostname && pid.toInt == version.pid) {
        // success!
        debug("Sucessfully acquired lock: " + taskEnv.lockFile)
        return;
      } else {
        // we're still waiting on the lock from some other process
        debug("Waiting for lock held by %s:%d: %s".format(hostname, pid, taskEnv.lockFile))
        Thread.sleep(LOCK_POLLING_TIMEOUT_SECS * 1000)
        acquireLock(taskEnv)
      }
    } else {
      debug("Claiming newly released lock: " + taskEnv.lockFile)
      writeLock(taskEnv)
      debug("Checking to ensure locking was successful: " + taskEnv.lockFile)
      acquireLock(taskEnv)
    }
  }
  
  def writeLock(taskEnv: TaskEnvironment) {
    debug("Writing lock: " + taskEnv.lockFile)
    Files.write("%s:%d".format(version.hostname, version.pid), taskEnv.lockFile)
    Files.write(version.version.toString, taskEnv.versionFile)
  }
  
  def releaseLock(taskEnv: TaskEnvironment) {
    debug("Releasing lock: " + taskEnv.lockFile)
    taskEnv.lockFile.delete()
  }
}