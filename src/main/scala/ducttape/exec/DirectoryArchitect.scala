package ducttape.exec

import java.io.File
import ducttape.workflow.Realization
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.Unbound
import ducttape.syntax.AbstractSyntaxTree.Literal
import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
import ducttape.syntax.AbstractSyntaxTree.Spec

class DirectoryArchitect(val workflowBaseDir: File,
                         val confBaseDir: File,
                         val confName: Option[String]) {

  val xdotFile = new File(confBaseDir, ".xdot")

  def assignPackedDir(taskName: String): File = {
    new File(confBaseDir, taskName).getAbsoluteFile
  }

  def assignUnversionedDir(taskName: String, realization: Realization): File = {
    val packedDir = assignPackedDir(taskName)
    new File(packedDir, realization.toString).getAbsoluteFile
  }

  // TODO: Rename to assignRealTaskDir
  // version: workflow version
  def assignDir(taskDef: TaskDef, realization: Realization, version: Int): File = {
    val unversionedDir = assignUnversionedDir(taskDef.name, realization)
    new File(unversionedDir, version.toString)
  }

  def assignBuildDir(packageName: String, packageVersion: String): File = {
    new File(confBaseDir, ".packages/%s/%s".format(packageName, packageVersion))
  }

  def assignOutFile(spec: Spec, taskDef: TaskDef, realization: Realization, version: Int): File = {
    //println("Assigning outfile for " + spec)
    val taskDir = assignDir(taskDef, realization, version)
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
                version: Int,
                srcSpec: Spec,
                srcTaskDef: TaskDef,
                srcRealization: Realization,
                srcVersion: Int): File = {

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
      case _ => assignOutFile(srcSpec, srcTaskDef, srcRealization, srcVersion)
    }
  }
  
  def getTempActionDir(actionName: String) = {
    val f = File.createTempFile("ducttape", actionName)
    f.delete() // delete file
    f.mkdirs() // and make it a directory instead
    f
  }
}
