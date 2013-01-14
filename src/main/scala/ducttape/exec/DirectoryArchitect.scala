package ducttape.exec

import java.io.File
import ducttape.workflow.Realization
import ducttape.syntax.Namespace
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.Unbound
import ducttape.syntax.AbstractSyntaxTree.Literal
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.FileFormatException
import ducttape.util.Environment
import ducttape.util.Files
import ducttape.workflow.Task
import ducttape.workflow.RealTask
import ducttape.workflow.VersionedTask

import grizzled.slf4j.Logging

class DirectoryArchitect(val flat: Boolean,
                         val versionedTasks: Boolean,
                         val workflowBaseDir: File,
                         val confName: Option[String]) extends Logging {

  val builtinsDir = new File(Environment.InstallDir, "builtins")
  
  val confBaseDir = workflowBaseDir
  
  val versionHistoryDir = new File(confBaseDir, ".versions")
  def assignVersionDir(workflowVersion: Int) = new File(versionHistoryDir, workflowVersion.toString)
  
  val xdotFile = new File(confBaseDir, ".xdot")
  val dbFile = new File(confBaseDir, ".db")

  def assignPackedDir(taskName: Namespace, relativeTo: File = confBaseDir): File = {
    // note: taskName will include various group names delimited by / such that they will create subdirectories
    new File(relativeTo, taskName.toString).getAbsoluteFile
  }
  
  // assign a version and realization-specific task directory (convenience method)
  def assignDir(task: VersionedTask): File = assignDir(task, relativeTo=confBaseDir)

  // assign a version and realization-specific task directory (convenience method)
  def assignDir(task: VersionedTask, relativeTo: File): File
    = assignDir(task.taskDef, task.realization, task.version, relativeTo, task.realization.toString)
  
  // assign a version and realization-specific task directory (convenience method)
  def assignDir(taskDef: TaskDef, realization: Realization, workflowVersion: Int,
                relativeTo: File = confBaseDir): File
    = assignDir(taskDef, realization, workflowVersion, relativeTo, realization.toString)
  
  // assign a version and realization-specific task directory
  private def assignDir(taskDef: TaskDef,
                        realization: Realization,
                        workflowVersion: Int,
                        relativeTo: File,
                        realName: String): File = {

    val packedDir = assignPackedDir(taskDef.name, relativeTo)
    if (flat) {
      if (realization != Task.NO_REALIZATION) { // TODO: Statically check this elsewhere, too?
        throw new FileFormatException("workflow may not contain any branchpoints if flat structure is being used", taskDef)
      }
      packedDir
    } else { // using hyper structure
      val realizationDir = new File(packedDir, realName).getAbsoluteFile
      if (versionedTasks) {
        new File(realizationDir, workflowVersion.toString).getAbsoluteFile
      } else {
        realizationDir
      }
    }
  }

  // this symlink includes *all* branch points in the current workflow, even
  //   if they're baseline branches. this symlink may include more components
  //   as the workflow evolves, but should be used ONLY by the user, not
  //   internally by ducttape
  // will return None if we are using flat structure since we will never
  //   have multiple realizations there OR if the symlink would be the same as the original dir
  def assignLongSymlink(task: VersionedTask): Option[File] = {
    if (flat) {
      None
    } else {
      val orig = assignDir(task.taskDef, task.realization, task.version, confBaseDir, task.realization.toString)
      val link = assignDir(task.taskDef, task.realization, task.version, confBaseDir, task.realization.toFullString())
      if (orig.getAbsolutePath == link.getAbsolutePath || task.realization.hasSingleBranchBaseline) {
        None
      } else {
        Some(link)
      }
    }  
  }
  
  val atticDir = new File(confBaseDir, ".attic")
  def assignAtticDir(task: VersionedTask)
    = assignDir(task, relativeTo=new File(atticDir, task.version.toString))

  def assignPackagesDir() = new File(confBaseDir, ".packages")

  // the directory where various versions of a software package will get built
  def assignBuildPackageDir(packageName: Namespace): File = {
    new File(assignPackagesDir(), packageName.toString)
  }
  
  def assignBuildHeadFile(packageName: Namespace) = new File(assignBuildPackageDir(packageName), "HEAD")

  // the directory where a specific version of a software package will get built
  def assignBuildDir(packageName: Namespace, packageVersion: String): File = {
    val packageDir = assignBuildPackageDir(packageName)
    new File(packageDir, packageVersion)
  }

  def assignOutFile(spec: Spec, taskDef: TaskDef, realization: Realization, taskVersion: Int): File = {
    debug(s"Assigning outfile for ${spec}")
    val taskDir = assignDir(taskDef, realization, taskVersion)

    spec.rval match {
      case Unbound() => { // user didn't specify a name for this output file
        new File(taskDir, spec.name) // will never collide with stdout.txt since it can't contain dots
      }
      case Literal(filename) => { // the user told us what name to use for the file
        new File(taskDir, filename)
      }
    }
  }

  def isAbsolute(path: String) = new File(path).isAbsolute

  def resolveLiteralPath(spec: LiteralSpec): File = {
    val path = spec.rval.value
    isAbsolute(path) match {
      case true => new File(path)
      // relative paths are resolved relative to the directory
      // **of the file in which this literal was declared**
      // this could be the workflow file or the config file
      case false => new File(spec.declaringFile.getParentFile, path)
    }
  }
  
  def getInFile(mySpec: Spec,
                realization: Realization,
                srcSpec: Spec,
                srcTaskDefOpt: Option[TaskDef],
                srcRealization: Realization,
                srcVersion: Int): File = {

    srcTaskDefOpt match {
      // no source task? this better be a literal
      case None => srcSpec.rval match {
        // gah, erasure!
        case Literal(path) => resolveLiteralPath(srcSpec.asInstanceOf[LiteralSpec])
        case _ => throw new RuntimeException(s"No source task found for spec ${mySpec} with source ${srcSpec}")
      }
      
      // has a source task? just recover the output path in the same way as when we originally produced it
      case Some(srcTaskDef) => assignOutFile(srcSpec, srcTaskDef, srcRealization, srcVersion)
    }
  }
  
  def getTempActionDir(actionName: String) = {
    val f = File.createTempFile("ducttape", actionName)
    f.delete() // delete file
    Files.mkdirs(f) // and make it a directory instead
    f
  }
}
