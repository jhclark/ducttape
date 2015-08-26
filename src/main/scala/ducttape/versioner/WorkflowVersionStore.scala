// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.versioner

import collection._
import java.io.File
import ducttape.exec.PackageVersioner
import ducttape.syntax.Namespace
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.exec.DirectoryArchitect
import ducttape.util.Files
import ducttape.util.Environment
import ducttape.util.Strings
import ducttape.hyperdag.PackedVertex
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.TaskTemplate
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
class WorkflowVersionStore(val tapeFile: File, // includes all imported files, the config file, and user directives, flattened together
                           val hostname: String,
                           val pid: Int,
                           val version: Int,
                           val existing: Seq[VersionedTaskId],
                           val todo: Seq[VersionedTaskId],
                           val packages: Seq[VersionedPackageId],
                           val packageDeps: Seq[(VersionedTaskId,VersionedPackageId)])
    extends WorkflowVersionInfo {

  lazy val taskVersions: Map[RealTaskId,Int] = WorkflowVersionStore.toMap(existing, todo)
  override def apply(task: RealTaskId): Int = taskVersions(task)
  override def get(task: RealTaskId): Option[Int] = taskVersions.get(task)

  override def toString() = s"${version}"
}

object WorkflowVersionStore {
  private[versioner] def toMap(existing: Seq[VersionedTaskId], todo: Seq[VersionedTaskId]) = {
    (existing ++ todo).map { t => (t.realTaskId, t.version) }.toMap
  }

  def load(versionInfoDir: File): WorkflowVersionStore = {
    val workflowCopy = new File(versionInfoDir, "workflow.tape")
    
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
    val packages = new mutable.ArrayBuffer[VersionedPackageId]
    val packageDeps = new mutable.ArrayBuffer[(VersionedTaskId,VersionedPackageId)]

    val packageMap = new mutable.HashMap[String,VersionedPackageId]
    
    for (line <- Files.read(manifestFile)) {
      if (!line.startsWith("#")) {
        val (lineType, remainingOpt) = Strings.splitOnFirst(line, '\t')
        val remaining = remainingOpt.get
        lineType match {
          case "package" => {
            val Seq(packageName, ver) = Strings.splitOn(remaining, "\t")
            val packageVer = new VersionedPackageId(packageName, ver)
            packages += packageVer
            packageMap += packageName -> packageVer
          }
          case "existing" => {
            val Seq(name, real, ver) = Strings.splitOn(remaining, "/")
            // TODO: XXX: HACK: Lane: Make sure this handles namespaces appropriately
            val namespace = Namespace.fromString(name)
            existing += new VersionedTaskId(namespace, real, ver.toInt)
          }
          case "todo" => {
            val (taskInfo, dependenciesOpt) = Strings.splitOnFirst(remaining, '\t')
            val Seq(name, real, ver) = Strings.splitOn(taskInfo, "/")
            // TODO: XXX: HACK: Lane: Make sure this handles namespaces appropriately
            val namespace = Namespace.fromString(name)
            val taskVer = new VersionedTaskId(namespace, real, ver.toInt)
            todo += taskVer

            val packageDepSeq: Seq[String] = dependenciesOpt.getOrElse("").trim match {
              case "" => Nil
              case str @ _ => Strings.splitOn(str, " ")
            }
            for (packageName <- packageDepSeq) {
              val packageVer: VersionedPackageId = packageMap(packageName)
              packageDeps += ((taskVer, packageVer))
            }
          }
          case _ => throw new RuntimeException(s"Invalid line type in file ${manifestFile.getAbsolutePath}: ${lineType}")
        }
      }
    }
    
    new WorkflowVersionStore(workflowCopy, hostname, pid, version, existing, todo, packages, packageDeps)
  }

  /** Returns tasks in the form "taskName/realizationName or packages in the form "packageName"
   *  NOTE: Currently, this does *NOT* return any task parents */
  def dependencies(workflow: HyperWorkflow, todoTask: VersionedTaskId): Seq[String] = {
    // TODO: XXX: Use a map instead of traversal. This is SLOW.
    //workflow.packedWalker.iterator.find { v: PackedVertex[Option[TaskTemplate]] =>
    workflow.dag.vertices.find { v: PackedVertex[Option[TaskTemplate]] =>
      val taskT: TaskTemplate = v.value.get
      taskT.name == todoTask.name
    } match {
      case Some(v) => {
        val taskT: TaskTemplate = v.value.get
        val packageNames: Seq[String] = taskT.packages.map(_.name)
        // TODO: Find task's parent tasks (this has to have knowledge of an unpacked traversal)
        packageNames
      }
      case None => throw new RuntimeException(s"Task not found: ${todoTask.name}")
    }
  }
  
  // create the next version of the workflow history and write it to disk
  // This is called by TentativeWorkflowVersionInfo.commit()
  // TODO: There should be a disk lock during this to prevent a race condition
  def create(dirs: DirectoryArchitect,
             workflow: HyperWorkflow,
             history: WorkflowVersionHistory,
             packages: Seq[VersionedPackageId],
             packageDeps: Seq[(VersionedTaskId,VersionedPackageId)],
             existing: Seq[VersionedTaskId],
             todo: Seq[VersionedTaskId]): WorkflowVersionStore = {

    val myVersionDir = dirs.assignVersionDir(history.nextVersion)
    
    if (myVersionDir.exists) {
      throw new RuntimeException(s"Version history directory already exists (this is probably a bug in ducttape. please report it. for now, you can try deleting the directory): ${myVersionDir.getAbsolutePath}")
    }
    Files.mkdirs(myVersionDir)
    
    // tape files may always be concatenated to preserve their original meaning,
    // with the exception that we must ignore imports when reloading
    // TODO: Remove import lines instead of only concatenating here
    // TODO: Insert comments that indicate which file is which
    val workflowCopy = new File(myVersionDir, "workflow.tape")
    Files.cat(workflow.wd.files, workflowCopy, separator="### $FILENAME ###", variable="$FILENAME")
    
    val hostname = Environment.hostname
    val pid = Environment.pid
    val pidFile = new File(myVersionDir, "pid.txt")
    Files.write(s"${hostname}:${pid}", pidFile)

    // create a manifest file detailing which packages, tasks, and workflow versions were used
    // TODO: XXX: We can't be sure what version will be used at this point due to competing process locks
    // NOTE: VersionedTaskId's are guaranteed to have their realizations in canonical form
    // TODO: Check for possible bugs in namespace handling with packages
    val manifest: Seq[String]
      = packages.map { p => s"package\t${p.packageName}\t${p.packageVersion}" } ++
        existing.map { task: VersionedTaskId => s"existing\t${task.name}/${task.realization}/${task.version}" } ++
        todo.map { task: VersionedTaskId => s"todo\t${task.name}/${task.realization}/${task.version}\t${dependencies(workflow,task).mkString(" ")}" }
    val manifestFile = new File(myVersionDir, "manifest.txt")
    Files.write(manifest, manifestFile)
     
    new WorkflowVersionStore(workflowCopy, hostname, pid, history.nextVersion, existing, todo, packages, packageDeps)
  }
}
