// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import ducttape.util.BashException
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.syntax.Namespace
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
      val packageNamespace: Namespace = myPackage.name // may contain slash delimited namespace
      val packageName: String = packageNamespace.toString
      val version: String = packageVersions(packageNamespace)
      val buildEnv = new BuildEnvironment(dirs, version, packageNamespace)

      // TODO: XXX: Can build ever interfere with another running workflow?
      if (buildEnv.buildDir.exists) {
        System.err.println(s"Removing incomplete package build: ${buildEnv.buildDir.toString}")
        Files.deleteDir(buildEnv.buildDir)
      }
      
      System.err.println(s"Checking out tool ${packageName} into ${buildEnv.buildDir}")
      packageVersions.checkout(myPackage, buildEnv.buildDir)

      // TODO: Check when the build code changes
      
      System.err.println(s"Building tool ${packageName} in ${buildEnv.buildDir}")
      val buildCmds = Seq(myPackage.commands.toString)
      // package params have already been checked to be literal
      val env: Seq[(String, String)] = myPackage.params.filter(!_.dotVariable).map(_.asInstanceOf[LiteralSpec]).map {
        spec => (spec.name, spec.rval.value)
      } 
      val stdPrefix = "build " + packageName
      val exitCode = Shell.run(buildCmds, stdPrefix, buildEnv.buildDir, env,
                               buildEnv.buildStdoutFile, buildEnv.buildStderrFile)
      Files.write(s"${exitCode}", buildEnv.buildExitCodeFile)
      if (exitCode != 0) {
        // just bail out, this workflow is doomed without its tools
        throw new BashException(s"Build task ${packageName} returned ${exitCode}")
      }
      packageVersions.writeHeadVersion(myPackage, version)
    }
  }
}
