package ducttape.exec

import ducttape.Config
import ducttape.workflow.hacks.Gimme
import ducttape.util.Shell
import ducttape.util.Files
import ducttape.syntax.AbstractSyntaxTree.PackageDef

class PackageBuilder(conf: Config,
                     dirs: DirectoryArchitect,
                     workflowVersion: Int) {
  
  def build(packages: Iterable[PackageDef]) {
    for(myPackage <- packages) {
      val buildEnv = new BuildEnvironment(dirs, workflowVersion, myPackage.name)
      System.err.println("%sBuilding tools for: %s in %s%s".format(conf.taskColor, myPackage.name, buildEnv.buildDir, Console.RESET))
      // TODO: XXX: Can build ever interfere with another running workflow?
      if(buildEnv.buildDir.exists) {
         System.err.println("Removing incomplete package build: %s".format(buildEnv.buildDir.toString))
        Files.deleteDir(buildEnv.buildDir)
      }
      buildEnv.buildDir.mkdirs()

      // TODO: XXX: Resolve the versioner and then get the checkout command
      // TODO: Run static analysis on package code
      // TODO: Verify that package blocks have the correct elements (as preproc, not now!)
      // TODO: Make sure package def didn't include packages, inputs, or outputs
      // TODO: Check when the build code changes
      
      val buildCmds = Seq(myPackage.commands.toString)
      val env = Seq()
      val exitCode = Shell.run(buildCmds, buildEnv.buildDir, env, buildEnv.buildStdoutFile, buildEnv.buildStderrFile)
      if(exitCode != 0) {
        // just bail out, this workflow is doomed without its tools
        System.err.println("%sERROR: Build task %s returned %s%s".format(conf.errorColor, myPackage.name, exitCode, Console.RESET))
        System.exit(1)
      }
    }
  }
}
