package ducttape.versioner

import collection._
import java.io.File
import ducttape.syntax.Namespace
import ducttape.exec.DirectoryArchitect
import ducttape.util.Files
import ducttape.util.Environment
import ducttape.util.Strings
import ducttape.workflow.RealTaskId
import ducttape.workflow.VersionedTaskId
import ducttape.workflow.Realization

// TODO: Double-check that this interacts reasonably with imports and config files
// then document how it does

/**
 * This will be implemented as a directory in the $CONF directory, which in turn stores:
 * * a copy of the .tape file
 * * and .conf file used to run the workflow
 * * the command line invocation of ducttape
 * * which vertices were both planned and already existing before running this workflow version
 * * which vertices were both planned and todo before running this workflow version
 * * the PID and hostname of the managing process
 * * the version number assigned to this run of the workflow
 *
 * Some critical classes for understanding versions: WorkflowVersionHistory, WorkflowVersionInfo,
 * and CompletionChecker (where we decide whether to reuse an existing version or if we'll need a
 * new one)
 */
class WorkflowVersionStore(val tapeFile: File,
                           val confFile: Option[File],
                           val hostname: String,
                           val pid: Int,
                           val version: Int,
                           val existing: Seq[VersionedTaskId],
                           val todo: Seq[VersionedTaskId])
    extends WorkflowVersionInfo {

  lazy val taskVersions: Map[RealTaskId,Int] = {
    (existing ++ todo).map { t => (t.realTaskId, t.version) }.toMap
  }
  override def apply(task: RealTaskId): Int = taskVersions(task)
  override def get(task: RealTaskId): Option[Int] = taskVersions.get(task)

  override def toString() = "%s".format(version)
}

object WorkflowVersionStore {
  def load(versionInfoDir: File): WorkflowVersionStore = {
    val workflowCopy = new File(versionInfoDir, "workflow.tape")
    val confCopy: Option[File] = Files.ls(versionInfoDir).find(_.getName.endsWith(".conf"))
    
    val pidFile = new File(versionInfoDir, "pid.txt")
    val (hostname, pid) = Files.read(pidFile).headOption match {
      case Some(line) => line.split(":") match {
        case Array(hostname, pid) => (hostname, pid.toInt)
          case _ => throw new RuntimeException(s"Malformed pid file: ${pidFile.getAbsolutePath}")
      }
       case None => throw new RuntimeException(s"Empty pid file: ${pidFile.getAbsolutePath}")
     }
     val version = versionInfoDir.getName.toInt

     // TODO: What if manifest doesn't exist? Directive indicating old mode?
     val manifestFile = new File(versionInfoDir, "manifest.txt")
     val existing = new mutable.ArrayBuffer[VersionedTaskId]
     val todo = new mutable.ArrayBuffer[VersionedTaskId]
     for (line <- Files.read(manifestFile)) {
      val Seq(taskInfo, status) = Strings.splitOn(line, "\t")
      val Seq(name, real, ver) = Strings.splitOn(taskInfo, "/")
      // TODO: XXX: HACK: Lane: Make sure this handles namespaces appropriately
      val namespace = Namespace.fromString(name)
      val id = new VersionedTaskId(namespace, real, ver.toInt)
      status match {
        case "existing" => existing += id
        case "todo" => todo += id
        case _ => throw new RuntimeException(s"Invalid task status in file ${manifestFile.getAbsolutePath}: ${status}")
      }
    }
    
    new WorkflowVersionStore(workflowCopy, confCopy, hostname, pid, version, existing, todo)
  }
  
  // create the next version of the workflow history and write it to disk
  // TODO: There should be a disk lock during this to prevent a race condition
  // TODO: Replace this with a "commit" method
  def create(dirs: DirectoryArchitect,
             workflowFile: File,
             confFile: Option[File],
             history: WorkflowVersionHistory,
             existing: Seq[VersionedTaskId],
             todo: Seq[VersionedTaskId]): WorkflowVersionStore = {

    val myVersionDir = dirs.assignVersionDir(history.nextVersion)
    
    if (myVersionDir.exists) {
      throw new RuntimeException(s"Version history directory already exists (this is probably a bug in ducttape. please report it. for now, you can try deleting the directory): ${myVersionDir.getAbsolutePath}")
    }
    Files.mkdirs(myVersionDir)
    
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
    Files.write(s"${hostname}:${pid}", pidFile)
    
    // TODO: Save args?
    new WorkflowVersionStore(workflowCopy, confCopy, hostname, pid, history.nextVersion, existing, todo)
  }
}
