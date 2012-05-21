package ducttape.exec

import collection._
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.syntax.AbstractSyntaxTree.ActionDef
import ducttape.syntax.AbstractSyntaxTree.PackageDef
import ducttape.syntax.AbstractSyntaxTree.VersionerDef
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.FileFormatException
import java.io.File
import ducttape.util.Files
import ducttape.util.Shell
import ducttape.util.BashException

object Versioners {
  def getVersioner(packageDef: PackageDef, versionerDefs: Map[String, VersionerDef]): VersionerDef = {
    val dotVars: Seq[LiteralSpec] = packageDef.params.filter { spec => spec.dotVariable }.map(_.asInstanceOf[LiteralSpec])
  
    val versionerName: String = dotVars.find { spec => spec.name == "versioner" } match {
      case Some(spec) => spec.asInstanceOf[LiteralSpec].rval.value
      case None => throw new FileFormatException(
        "No versioner specified for package %s".format(packageDef.name), packageDef)
    }
    
    val versionerDef: VersionerDef = versionerDefs.get(versionerName) match {
      case Some(v) => v
      case None => throw new FileFormatException(
        "Versioner not defined '%s' for package '%s'".format(versionerName, packageDef.name), packageDef)
    }
    versionerDef
  }
}

// throws FileFormatException if required versioner, or actions are not defined
class PackageVersionerInfo(val versionerDef: VersionerDef) { 
  val actionDefs: Seq[ActionDef] = versionerDef.blocks.collect { case x: ActionDef => x }.filter(_.keyword == "action")
  val checkoutDef: ActionDef = actionDefs.find { a => a.name == "checkout" } match {
    case Some(v) => v
    case None => throw new FileFormatException(
      "Checkout action not defined for versioner '%s'".format(versionerDef.name), versionerDef)
  }
  val repoVersionDef: ActionDef = actionDefs.find { a => a.name == "repo_version" } match {
    case Some(v) => v
    case None => throw new FileFormatException(
      "repo_version action not defined for versioner '%s'".format(versionerDef.name), versionerDef)
  }
  val localVersionDef: ActionDef = actionDefs.find { a => a.name == "local_version" } match {
    case Some(v) => v
    case None => throw new FileFormatException(
      "local_version action not defined for versioner '%s'".format(versionerDef.name), versionerDef)
  }
  
  val versionerEnv: Seq[(String,String)] = {
    val dotVars: Seq[LiteralSpec] = versionerDef.params.filter(_.dotVariable).map(_.asInstanceOf[LiteralSpec])
    dotVars.map { spec => (spec.name, spec.rval.value) }
  }
}

class PackageVersioner(val dirs: DirectoryArchitect,
                       val versioners: Seq[VersionerDef]) {
  
  private val versionerDefs = versioners.map { v => (v.name, v) }.toMap
  
  // the following 3 fields get populated by findAlreadyBuilt()
  val packageVersions = new mutable.HashMap[String,String]
  var packagesToBuild: Seq[PackageDef] = Nil
  var packagesExisting: Seq[PackageDef] = Nil
  
  def apply(packageName: String) = packageVersions(packageName)
  
  /**
   * Returns a tuple of (alreadyBuilt, needsBuilding)
   */
  def findAlreadyBuilt(packageDefs: Seq[PackageDef]) {
    val (alreadyDone, notDone) = packageDefs.partition(isAlreadyBuilt)
    packagesExisting = alreadyDone
    packagesToBuild = notDone
  }
  
  private def isAlreadyBuilt(packageDef: PackageDef): Boolean = {
    val versionerDef: VersionerDef = Versioners.getVersioner(packageDef, versionerDefs)
    val info = new PackageVersionerInfo(versionerDef)
    
    // TODO: do static analysis on all actions
    // TODO: Assign inputs and outputs to actions
    // TODO: Check actions to make sure the correct inputs/outputs/params are specified
    
    // TODO: Create an "ActionEnvironment" for these sorts of situations?
    val workDir = dirs.getTempActionDir("versioner.repo_version")
    val versionFile = new File(workDir, "repo_version.txt")
    val stdoutFile = new File(workDir, "stdout.txt")
    val stderrFile = new File(workDir, "stderr.txt")
    val exitCodeFile = new File(workDir, "exit_code.txt")
    
    // the environment also includes referenced dot variables from the package
    val env = Seq( ("version", versionFile.getAbsolutePath) ) ++ info.versionerEnv
    
    val exitCode = Shell.run(info.repoVersionDef.commands.toString, workDir, env, stdoutFile, stderrFile)
    Files.write("%d".format(exitCode), exitCodeFile)
    if(exitCode != 0) {
      throw new BashException("Action repo_version for versioner %s for package %s (%s:%d) returned %s".format(
        info.versionerDef.name, packageDef.name, packageDef.declaringFile, packageDef.pos.line, exitCode))
    }
    
    val repoVersion = Files.read(versionFile).headOption match {
      case Some(v) => v
      case None => throw new BashException(
        "Action repo_version for versioner %s for package %s (%s:%d) returned a blank version".format(
           info.versionerDef.name, packageDef.name, packageDef.declaringFile, packageDef.pos.line))
    }
    
    System.err.println("Version is %s".format(repoVersion))
    packageVersions += packageDef.name -> repoVersion
    
    val buildDir = dirs.assignBuildDir(packageDef.name, repoVersion)
    // TOOD: Far more aggressive checking to see if build completed
    val exists = buildDir.exists
    System.err.println("Package %s: %s".format(packageDef.name, if (exists) "FOUND" else "NOT FOUND"))
    exists
    
    // 1) Get the version
    // 2) Save the repo version for later executor use
    // 3) Check if we've built that version already
    // 3) Create the appropriate directory later.
    // 4) Checkout the package later.
    // 5) Call the builder later.
  }
  
  def checkout(packageDef: PackageDef, buildDir: File) {
    val versionerDef = Versioners.getVersioner(packageDef, versionerDefs)
    val info = new PackageVersionerInfo(versionerDef)
    val repoVersion = packageVersions(packageDef.name)
    
    // TODO: Remove partial builds?
    
    val workDir = dirs.getTempActionDir("versioner.checkout")
    val stdoutFile = new File(workDir, "checkout_stdout.txt")
    val stderrFile = new File(workDir, "checkout_stderr.txt")
    val exitCodeFile = new File(workDir, "checkout_exit_code.txt")
    
    val env = Seq( ("dir", buildDir.getAbsolutePath) ) ++ info.versionerEnv
    
    System.err.println("Checking out %s into %s via %s".format(
      packageDef.name, buildDir.getAbsolutePath, workDir.getAbsolutePath))
    
    val exitCode = Shell.run(info.checkoutDef.commands.toString, workDir, env, stdoutFile, stderrFile)
    Files.write("%d".format(exitCode), exitCodeFile)
    if (exitCode != 0) {
      throw new BashException("Action repo_version for versioner %s returned %s".format(
        info.versionerDef.name, exitCode))
    }
    
    // TODO: Move stdout, etc into build directory for archival?
  }
}
