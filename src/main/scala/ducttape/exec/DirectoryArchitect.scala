package ducttape.exec

import java.io.File
import ducttape.workflow.Realization
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.Unbound
import ducttape.syntax.AbstractSyntaxTree.Literal
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
  
  val confBaseDir = confName match {
    case Some(name) => new File(workflowBaseDir, name)
    case None => workflowBaseDir
  }
  
  val versionHistoryDir = new File(confBaseDir, ".versions")
  def assignVersionDir(version: Int) = new File(versionHistoryDir, version.toString)
  
  val xdotFile = new File(confBaseDir, ".xdot")

  def assignPackedDir(taskName: String, relativeTo: File = confBaseDir): File = {
    new File(relativeTo, taskName).getAbsoluteFile
  }
  
  def assignDir(task: RealTask, relativeTo: File = confBaseDir): File = assignDir(task.taskDef, task.realization, relativeTo)
  
  def assignDir(taskDef: TaskDef, realization: Realization): File = assignDir(taskDef, realization, confBaseDir)
  
  def assignDir(taskDef: TaskDef, realization: Realization, relativeTo: File): File = {
    val packedDir = assignPackedDir(taskDef.name, relativeTo)
    if (flat) {
      if (realization != Task.NO_REALIZATION) { // TODO: Statically check this elsewhere, too?
        throw new FileFormatException("workflow may not contain any branchpoints if flat structure is being used", taskDef)
      }
      packedDir
    } else { // using hyper structure
      new File(packedDir, realization.toString).getAbsoluteFile 
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
      case Literal(filename) => { // the user told us what name to use for the file -- put it under work/
        new File(taskDir, "work/%s".format(filename))
      }
    }
  }

  def isAbsolute(path: String) = new File(path).isAbsolute

  def resolveLiteralPath(path: String): File = {
    isAbsolute(path) match {
      case true => new File(path)
      // relative paths are resolved relative to the workflow base dir (where the .tape file resides)
      // since the config directory might not even exist yet
      case false => new File(workflowBaseDir, path) // resolve relative paths relative to the workflow file (baseDir)
    }
  }
    
  def getInFile(mySpec: Spec,
                realization: Realization,
                srcSpec: Spec,
                srcTaskDef: TaskDef,
                srcRealization: Realization): File = {

    // first, resolve the realization, if necessary
    // also, resolve through any ConfigVariables
    // TODO: Move this into the realization unpacking code
    val realizedRval = mySpec.rval match {
      case BranchPointDef(_,_) => srcSpec.rval // the "source" internal to the branch declaration
      case ConfigVariable(_) => srcSpec.rval // the "source" accoding to the config variable and internal to any branch declaration
      case _ => mySpec.rval
    }

    //err.println("My realization is " + Task.realizationName(realization))
    //err.println("Src realization is " + Task.realizationName(srcRealization))

    // TODO: We should have already checked that this file exists by now?
    realizedRval match {
      case Literal(path) => resolveLiteralPath(path)
      // branches, variables, etc get matched on the src, which we already resolved
      case _ => assignOutFile(srcSpec, srcTaskDef, srcRealization)
    }
  }
  
  def getTempActionDir(actionName: String) = {
    val f = File.createTempFile("ducttape", actionName)
    f.delete() // delete file
    f.mkdirs() // and make it a directory instead
    f
  }
  
  // ==== TODO: Separate off the following UI-specific code =====
  
  def colorizeDir(taskName: String, real: Realization)(implicit conf: ducttape.Config): String = {
    val x = "%s/%s%s%s".format(confBaseDir.getAbsolutePath, conf.taskNameColor, taskName, conf.resetColor)           
    if (flat) {
      x
    } else {
      x + "/%s%s%s".format(conf.realNameColor, real.toString, conf.resetColor)
    }
  }

  def colorizeDirs(list: Iterable[RealTask])(implicit conf: ducttape.Config): Seq[String] = {
    list.toSeq.map{ task => colorizeDir(task.name, task.realization) }
  }
}
