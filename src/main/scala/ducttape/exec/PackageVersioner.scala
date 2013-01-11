package ducttape.exec

import collection._
import math.Ordering

import ducttape.cli.Directives
import ducttape.workflow.Realization
import ducttape.syntax.Namespace
import ducttape.syntax.AbstractSyntaxTree.ActionDef
import ducttape.syntax.AbstractSyntaxTree.PackageDef
import ducttape.syntax.AbstractSyntaxTree.VersionerDef
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.FileFormatException
import ducttape.syntax.Namespace
import ducttape.util.Files
import ducttape.util.Shell
import ducttape.util.BashException

import java.io.File
import grizzled.slf4j.Logging

object Versioners {
  def getVersioner(packageDef: PackageDef, versionerDefs: Map[Namespace, VersionerDef]): VersionerDef = {
    val dotVars: Seq[LiteralSpec] = packageDef.params.filter { spec => spec.dotVariable }.map(_.asInstanceOf[LiteralSpec])
  
    val versionerName: Namespace = dotVars.find { spec => spec.name == "versioner" } match {
      case Some(spec) => {
        val namespaceStr: String = spec.asInstanceOf[LiteralSpec].rval.value
        Namespace.fromString(namespaceStr)
      }
      case None => throw new FileFormatException(s"No versioner specified for package ${packageDef.name}", packageDef)
    }
    
    val versionerDef: VersionerDef = versionerDefs.get(versionerName) match {
      case Some(v) => v
      case None => throw new FileFormatException(s"Versioner not defined '${versionerName}' for package '${packageDef.name}'", packageDef)
    }
    versionerDef
  }
}

// throws FileFormatException if required versioner, or actions are not defined
class PackageVersionerInfo(val versionerDef: VersionerDef) extends Logging { 
  val actionDefs: Seq[ActionDef] = versionerDef.blocks.collect {
    case x: ActionDef => x
  }.filter(_.keyword == "action")

  val checkoutDef: ActionDef = actionDefs.find { a => a.name.toString == "checkout" } match {
    case Some(v) => v
    case None => throw new FileFormatException(
      s"Checkout action not defined for versioner '${versionerDef.name}", versionerDef)
  }
  val repoVersionDef: ActionDef = actionDefs.find { a => a.name.toString == "repo_version" } match {
    case Some(v) => v
    case None => throw new FileFormatException(
      s"repo_version action not defined for versioner '${versionerDef.name}'", versionerDef)
  }
  val localVersionDef: ActionDef = actionDefs.find { a => a.name.toString == "local_version" } match {
    case Some(v) => v
    case None => throw new FileFormatException(
      s"local_version action not defined for versioner '${versionerDef.name}'", versionerDef)
  }
  val requiredParams: Set[String] = versionerDef.params.map(_.name).toSet
  
  def getEnv(packageDef: PackageDef): Seq[(String,String)] = {
    val packageDotParams: Seq[LiteralSpec] = packageDef.params.filter(_.dotVariable).map(_.asInstanceOf[LiteralSpec])
    packageDotParams.map { spec => (spec.name, spec.rval.value) }
  }

  // do we have either a version_to_int action or a repo_history action?
  // a comparator is necessary to determine which previously built package
  // is most recent
  def getComparatorDef(): Option[ActionDef] = {
    val actions: Seq[ActionDef] = actionDefs.filter {
      action => action.name == "version_to_int" || action.name == "repo_history"
    }
    actions.size match {
      case 0 => None // TODO: Is it an error not to define one of these?
      case 1 => Some(actions(0))
      case _ => throw new FileFormatException(
        "Versioner should define exactly one of 'version_to_int' or 'repo_history'".
          format(versionerDef.name), versionerDef)
    }
  }
}

// we serialize package versions to disk and just try to reload them
// the package versions are the version control system's native versioning strings
// (e.g. sha1 hashes for git or 'r000' for SVN) separated by newlines
// with the newest versions being at the top of the file
class PackageVersionComparator(compareAction: ActionDef) {

  throw new RuntimeException("Unimplemented")

  // TODO: Create temporary action directory and get version history... depending
  // which comparator is defined
  val versionList: Seq[String] = Nil

  // this comparator does *NOT* allow versions to be equal
  def isANewerThanB(versionA: String, versionB: String): Boolean = {
    // we just do linear lookup since we're never looking at more than a few thousand versions
    // and we only do this lookup a tiny number of times per ducttape execution
    val idxA: Int = versionList.indexOf(versionA)
    val idxB: Int = versionList.indexOf(versionB)
    assert(idxA != idxB)
    // TODO: What about -1?
    return idxA < idxB
  }
  def isAOlderThanB(versionA: String, versionB: String) = !isANewerThanB(versionA, versionB)

  // ordering on string-typed versions
  val ordering: Ordering[String] = Ordering.fromLessThan(isAOlderThanB)
}

// TODO: Rename as "PackagesVersioner" or something to reflect
// that it manages the version of *all* packages
class PackageVersioner(val dirs: DirectoryArchitect,
                       val versioners: Seq[VersionerDef],
                       val forceUpdate: Boolean = false)
                      (implicit val directives: Directives) extends Logging {

  // we need a package version comparator if we might need to detect the current version
  // todo: create the Info classes to determine if we have comparators
  //require(forceUpgrade || directives.autoUpdatePackages || versionComparator.forall.isDefined)
  
  private val versionerDefs = versioners.map { v => (v.name, v) }.toMap
  
  // the following 3 fields get populated by findAlreadyBuilt() and isAlreadyBuild()
  // TODO: Change these to immutable and initialize them on object creation?
  val packageVersions = new mutable.HashMap[Namespace,String] // packageName -> repoVersion
  var packagesToBuild: Seq[PackageDef] = Nil
  var packagesExisting: Seq[PackageDef] = Nil // both existing *and* up-to-date
  
  def apply(packageName: Namespace) = packageVersions(packageName)
  
  /**
   * Returns a tuple of (alreadyBuilt, needsBuilding)
   */
  def findAlreadyBuilt(packageDefs: Seq[PackageDef]) {
    val (alreadyDone, notDone) = packageDefs.partition(isAlreadyBuilt)
    packagesExisting = alreadyDone
    packagesToBuild = notDone
  }
  
  // analyze all previous versions of a particular software package
  // to see if it's up-to-date
  private def isAlreadyBuilt(packageDef: PackageDef): Boolean = {
    // we only get previous version (which requires a comparator) if we
    // aren't forced to rebuild
    def getPreviousVersion(info: PackageVersionerInfo): Option[String] = {
      System.err.println("WARNING: Skipping previous package check (currently under development)")
      return None

      // First, we need either a way of comparing version hashes (git and friends)
      // or a way of turning revisions into integers (SVN)
      val comparatorDef: ActionDef = info.getComparatorDef() match {
        case Some(action) => action
        case None => throw new FileFormatException(
          "We need to determine the latest version of the package '%s' but versioner '%s' doesn't define a comparator of either 'version_to_int' or 'repo_history'".
            format(packageDef.name, info.versionerDef.name), info.versionerDef)
      }
      val comparator = new PackageVersionComparator(comparatorDef)

      // 1) list all existing directories/versions of this package
      val packageDir: File = dirs.assignBuildPackageDir(packageDef.name)
      val packageBuildDirs: Seq[File] = Files.ls(packageDir)

      // 2) Filter them by which builds are successful
      val prevBuilds: Seq[BuildEnvironment] = packageBuildDirs.map { versionDir: File =>
        val versionStr: String = versionDir.getName
        new BuildEnvironment(dirs, versionStr, packageDef.name)
      }
      val successfulBuilds: Seq[BuildEnvironment] = {
        prevBuilds.filter { buildEnv => PackageBuilder.isBuildSuccessful(buildEnv) }
      }

      // 3) sort the successful builds by the version comparator
      val sortedBuilds: Seq[String] = successfulBuilds.map(_.packageVersion).sorted(comparator.ordering)
      sortedBuilds.headOption
    }

    // if we're using auto-update mode, we must check if the version is current
    // otherwise, just check if this is the first time we've encountered this package
    val versionerDef: VersionerDef = Versioners.getVersioner(packageDef, versionerDefs)
    val info = new PackageVersionerInfo(versionerDef)
    if (directives.autoUpdatePackages || forceUpdate || getPreviousVersion(info).isEmpty) {
      // TODO: Assign inputs and outputs to actions
    
      // TODO: Create an "ActionEnvironment" for these sorts of situations?
      val repoVersion: String = {
        val workDir = dirs.getTempActionDir("versioner.repo_version")
        val versionFile = new File(workDir, "repo_version.txt")
        val stdoutFile = new File(workDir, "stdout.txt")
        val stderrFile = new File(workDir, "stderr.txt")
        val exitCodeFile = new File(workDir, "exit_code.txt")
        
        // the environment also includes referenced dot variables from the package
        val env = Seq( ("version", versionFile.getAbsolutePath) ) ++ info.getEnv(packageDef)
        debug("Environment for repo_version action is: " + env)
        
        val stdPrefix = versionerDef.name + " repo_version " + packageDef.name
        val exitCode = Shell.run(info.repoVersionDef.commands.toString, stdPrefix, workDir, env, stdoutFile, stderrFile)
        Files.write(s"${exitCode}", exitCodeFile)
        if (exitCode != 0) {
          throw new BashException(s"Action repo_version for versioner '${info.versionerDef.name}' for package '${packageDef.name}' " +
                                  s"(${packageDef.declaringFile}:${packageDef.pos.line}) returned ${exitCode}")
      }
        
        val repoVersion = Files.read(versionFile).headOption match {
          case Some(v) => v
          case None => throw new BashException(
            s"Action repo_version for versioner '${info.versionerDef.name}' for package '${packageDef.name}' " +
            s"(${packageDef.declaringFile}:${packageDef.pos.line}) returned a blank version")
        }
        
        Files.deleteDir(workDir) // workDir goes out of scope here as we delete it
        repoVersion
      }
      
      System.err.println(s"Package ${packageDef.name}: Repository version is ${repoVersion}")
      packageVersions += packageDef.name -> repoVersion
    
      // TODO: Different messages depending on build failed, etc.
      val buildEnv = new BuildEnvironment(dirs, repoVersion, packageDef.name)
      val exists = PackageBuilder.isBuildSuccessful(buildEnv)
      System.err.println(s"Package ${packageDef.name}: %s".format(if (exists) "ALREADY BUILT" else "VERSION NOT CURRENT"))
      exists
    } else {
      // we don't need to auto-update so just check if this is the first time we've had any need
      // to build this package
      getPreviousVersion(info) match {
        case Some(previouslyBuiltRepoVersion: String) => {
          packageVersions += packageDef.name -> previouslyBuiltRepoVersion
          true
        }
        case None => false
      }
    }
  }
  
  def checkout(packageDef: PackageDef, buildDir: File) {
    val versionerDef = Versioners.getVersioner(packageDef, versionerDefs)
    val info = new PackageVersionerInfo(versionerDef)
    val repoVersion = packageVersions(packageDef.name)
    
    // partial builds should already have been removed by PackageBuilder
    
    val workDir = dirs.getTempActionDir("versioner.checkout")
    val stdoutFile = new File(workDir, "checkout_stdout.txt") 
    val stderrFile = new File(workDir, "checkout_stderr.txt")
    val exitCodeFile = new File(workDir, "checkout_exit_code.txt")
    
    // TODO: "dir" is perhaps a bad choice of variable name here.
    // we should be checking for collissions
    val env = Seq( ("dir", buildDir.getAbsolutePath) ) ++ info.getEnv(packageDef)
    
    System.err.println(s"Checking out ${packageDef.name} into ${buildDir.getAbsolutePath} via ${workDir.getAbsolutePath}")

    Files.mkdirs(workDir)
    Files.mkdirs(buildDir)

    debug("Running checkout commands: " + info.checkoutDef.commands.toString)
    val stdPrefix = packageDef.name + " checkout " + info.versionerDef.name
    val exitCode = Shell.run(info.checkoutDef.commands.toString, stdPrefix, workDir, env, stdoutFile, stderrFile)
    Files.write("%d".format(exitCode), exitCodeFile)
    Files.moveFileToDir(stdoutFile, buildDir)
    Files.moveFileToDir(stderrFile, buildDir)
    Files.moveFileToDir(exitCodeFile, buildDir)
    
    Files.deleteDir(workDir)
    
    if (exitCode != 0) {
      throw new BashException(s"Action checkout for versioner ${info.versionerDef.name} returned ${exitCode}")
    }
  }
}
