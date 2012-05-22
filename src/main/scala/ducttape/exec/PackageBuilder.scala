package ducttape.exec

import ducttape.util.BashException
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.syntax.AbstractSyntaxTree.PackageDef

/**
 * If we determine that a package is out of date (the requested version is not
 * already built within ducttape), then we use this PackageBuilder to build
 * a newly checked-out copy.
 */
class PackageBuilder(dirs: DirectoryArchitect,
                     packageVersions: PackageVersioner) {
  
  def build(packages: Iterable[PackageDef]) {
    for (myPackage: PackageDef <- packages) {
      val buildEnv = new BuildEnvironment(dirs, packageVersions(myPackage.name), myPackage.name)
      System.err.println("Building tools %s in %s".format(myPackage.name, buildEnv.buildDir))
      // TODO: XXX: Can build ever interfere with another running workflow?
      if (buildEnv.buildDir.exists) {
         System.err.println("Removing incomplete package build: %s".format(buildEnv.buildDir.toString))
        Files.deleteDir(buildEnv.buildDir)
      }
      packageVersions.checkout(myPackage, buildEnv.buildDir)

      // TODO: XXX: Resolve the versioner and then get the checkout command
      // TODO: Run static analysis on package code
      // TODO: Verify that package blocks have the correct elements (as preproc, not now!)
      // TODO: Make sure package def didn't include packages, inputs, or outputs
      // TODO: Check when the build code changes
      
      val buildCmds = Seq(myPackage.commands.toString)
      val env = Seq()
      val exitCode = Shell.run(buildCmds, buildEnv.buildDir, env, buildEnv.buildStdoutFile, buildEnv.buildStderrFile)
      if (exitCode != 0) {
        // just bail out, this workflow is doomed without its tools
        new BashException("ERROR: Build task %s returned %s".format(myPackage.name, exitCode))
      }
    }
  }
}
