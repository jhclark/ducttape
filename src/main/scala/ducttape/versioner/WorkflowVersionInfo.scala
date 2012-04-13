package ducttape.versioner

import ducttape.workflow.Realization
import java.io.File
import ducttape.exec.DirectoryArchitect
import ducttape.util.Files
import ducttape.util.Environment

/**
 * This will be implemented as a directory in the $CONF directory, which in turn stores:
 * * a copy of the .tape file
 * * and .conf file used to run the workflow
 * * the command line invocation of ducttape
 * * which vertices were planned
 * * the PID and hostname of the managing process
 * * the version number assigned to that run of the workflow
 */
class WorkflowVersionInfo(val tapeFile: File,
                        val confFile: Option[File],
                        val hostname: String,
                        val pid: Int,
                        val version: Int)

object WorkflowVersionInfo {
  def load(versionInfoDir: File): WorkflowVersionInfo = {
     val workflowCopy = new File(versionInfoDir, "workflow.tape")
     val confCopy: Option[File] = Files.ls(versionInfoDir).find(_.getName.endsWith(".conf"))
     
     val pidFile = new File(versionInfoDir, "pid.txt")
     val (hostname, pid) = Files.read(pidFile).headOption match {
       case Some(line) => line.split(":") match {
         case Array(hostname, pid) => (hostname, pid.toInt)
         case _ => throw new RuntimeException("Malformed pid file: %s".format(pidFile.getAbsolutePath))
       }
       case None => throw new RuntimeException("Empty pid file: %s".format(pidFile.getAbsolutePath))
     }
     val version = versionInfoDir.getName.toInt
     new WorkflowVersionInfo(workflowCopy, confCopy, hostname, pid, version)
  }
  
  def create(dirs: DirectoryArchitect,
             workflowFile: File,
             confFile: Option[File],
             history: WorkflowVersionHistory): WorkflowVersionInfo = {
    
    val myVersionDir = dirs.assignVersionDir(history.nextVersion)
    
    if (myVersionDir.exists) {
      throw new RuntimeException("Version history directory already exists!")
    }
    myVersionDir.mkdirs()
    
    val workflowCopy = new File(myVersionDir, "workflow.tape")
    Files.copy(workflowFile, workflowCopy)
    
    val confCopy = confFile match {
      case Some(f: File) => {
        val copy = new File(myVersionDir, f.getName)
        Files.copy(f, copy)
        Some(copy)
      }
      case None => None
    }
    
    val hostname = Environment.hostname
    val pid = Environment.pid
    val pidFile = new File(myVersionDir, "pid.txt")
    Files.write("%s:%d".format(hostname, pid), pidFile)
    
    // TODO: Save args?
    new WorkflowVersionInfo(workflowCopy, confCopy, hostname, pid, history.nextVersion)
  }
}