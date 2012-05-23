package ducttape.exec

import ducttape.util.BashException
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.syntax.AbstractSyntaxTree.PackageDef
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec

import grizzled.slf4j.Logging

object PackageBuilder {
  def isBuildSuccessful(buildEnv: BuildEnvironment): Boolean
    = CompletionChecker.isExitCodeZero(buildEnv.buildExitCodeFile)
}

/**
 * If we determine that a package is out of date (the requested version is not
 * already built within ducttape), then we use this PackageBuilder to build
 * a newly checked-out copy.
 */
class PackageBuilder(dirs: DirectoryArchitect,
                     packageVersions: PackageVersioner) extends Logging {
  
  def build(packages: Iterable[PackageDef]) {
    for (myPackage: PackageDef <- packages) {
      val buildEnv = new BuildEnvironment(dirs, packageVersions(myPackage.name), myPackage.name)

      // TODO: XXX: Can build ever interfere with another running workflow?
      if (buildEnv.buildDir.exists) {
        System.err.println("Removing incomplete package build: %s".format(buildEnv.buildDir.toString))
        Files.deleteDir(buildEnv.buildDir)
      }
      
      System.err.println("Checking out tool %s into %s".format(myPackage.name, buildEnv.buildDir))
      packageVersions.checkout(myPackage, buildEnv.buildDir)

      // TODO: Check when the build code changes
      
      System.err.println("Building tool %s in %s".format(myPackage.name, buildEnv.buildDir))
      val buildCmds = Seq(myPackage.commands.toString)
      // package params have already been checked to be literal
      val env: Seq[(String, String)] = myPackage.params.filter(!_.dotVariable).map(_.asInstanceOf[LiteralSpec]).map {
        spec => (spec.name, spec.rval.value)
      } 
      val stdPrefix = "build " + myPackage.name
      val exitCode = Shell.run(buildCmds, stdPrefix, buildEnv.buildDir, env,
                               buildEnv.buildStdoutFile, buildEnv.buildStderrFile)
      Files.write("%d".format(exitCode), buildEnv.buildExitCodeFile)
      if (exitCode != 0) {
        // just bail out, this workflow is doomed without its tools
        throw new BashException("ERROR: Build task %s returned %s".format(myPackage.name, exitCode))
      }
    }
  }
}
