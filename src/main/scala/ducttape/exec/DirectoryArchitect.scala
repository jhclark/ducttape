package ducttape.exec

import java.io.File
import ducttape.workflow.Realization
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.Unbound
import ducttape.syntax.AbstractSyntaxTree.Literal
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.util.Environment
import ducttape.workflow.Task
import ducttape.syntax.FileFormatException
import ducttape.workflow.RealTask

class DirectoryArchitect(val flat: Boolean,
                         val workflowBaseDir: File,
                         val confName: Option[String]) {

  val installDir = Environment.getJarDir
  val builtinsDir = new File(installDir, "builtins")
  
  val confBaseDir = workflowBaseDir
  
  val versionHistoryDir = new File(confBaseDir, ".versions")
  def assignVersionDir(version: Int) = new File(versionHistoryDir, version.toString)
  
  val xdotFile = new File(confBaseDir, ".xdot")
  val dbFile = new File(confBaseDir, ".db")

  def assignPackedDir(taskName: String, relativeTo: File = confBaseDir): File = {
    new File(relativeTo, taskName).getAbsoluteFile
  }
  
  def assignDir(task: RealTask, relativeTo: File = confBaseDir): File
    = assignDir(task.taskDef, task.realization, relativeTo, task.realization.toString)
  
  def assignDir(taskDef: TaskDef, realization: Realization): File
    = assignDir(taskDef, realization, confBaseDir, realization.toString)
  
  private def assignDir(taskDef: TaskDef, realization: Realization, relativeTo: File, realName: String): File = {
    val packedDir = assignPackedDir(taskDef.name, relativeTo)
    if (flat) {
      if (realization != Task.NO_REALIZATION) { // TODO: Statically check this elsewhere, too?
        throw new FileFormatException("workflow may not contain any branchpoints if flat structure is being used", taskDef)
      }
      packedDir
    } else { // using hyper structure
      new File(packedDir, realName).getAbsoluteFile 
    }
  }

  // this symlink includes *all* branch points in the current workflow, even
  //   if they're baseline branches. this symlink may include more components
  //   as the workflow evolves, but should be used ONLY by the user, not
  //   internally by ducttape
  // will return None if we are using flat structure since we will never
  //   have multiple realizations there OR if the symlink would be the same as the original dir
  def assignLongSymlink(task: RealTask): Option[File] = {
    if (flat) {
      None
    } else {
      val orig = assignDir(task.taskDef, task.realization, confBaseDir, task.realization.toString)
      val link = assignDir(task.taskDef, task.realization, confBaseDir, task.realization.toFullString)
      if (orig.getAbsolutePath == link.getAbsolutePath || task.realization.hasSingleBranchBaseline) {
        None
      } else {
        Some(link)
      }
    }  
  }
  
  val atticDir = new File(confBaseDir, ".attic")
  def assignAtticDir(task: RealTask, version: Int)
    = assignDir(task, relativeTo=new File(atticDir, version.toString)) 

  def assignBuildDir(packageName: String, packageVersion: String): File = {
    new File(confBaseDir, ".packages/%s/%s".format(packageName, packageVersion))
  }

  def assignOutFile(spec: Spec, taskDef: TaskDef, realization: Realization): File = {
    //println("Assigning outfile for " + spec)
    val taskDir = assignDir(taskDef, realization)
    assert(!spec.isInstanceOf[BranchPointDef])

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
                srcRealization: Realization): File = {

    srcTaskDefOpt match {
      // no source task? this better be a literal
      case None => srcSpec.rval match {
        // gah, erasure!
        case Literal(path) => resolveLiteralPath(srcSpec.asInstanceOf[LiteralSpec])
        case _ => throw new RuntimeException("No source task found for spec %s with source %s ".format(mySpec, srcSpec))
      }
      
      // has a source task? just recover the output path in the same way as when we originally produced it
      case Some(srcTaskDef) => assignOutFile(srcSpec, srcTaskDef, srcRealization) 
    }
  }
  
  def getTempActionDir(actionName: String) = {
    val f = File.createTempFile("ducttape", actionName)
    f.delete() // delete file
    f.mkdirs() // and make it a directory instead
    f
  }
}
