package ducttape.exec

import ducttape.Config
import ducttape.workflow.hacks.Gimme
import ducttape.util.Shell
import ducttape.util.Files

class PackageBuilder(conf: Config, dirs: DirectoryArchitect, workflowVersion: Int) {
  def build(packageNames: Iterable[String]) {
    for(packageName <- packageNames) {
      val buildEnv = new BuildEnvironment(dirs, workflowVersion, packageName)
      println("%sBuilding tools for: %s in %s%s".format(conf.taskColor, packageName, buildEnv.buildDir, Console.RESET))
      // TODO: XXX: Can build ever interfere with another running workflow?
      println("Removing: %s".format(buildEnv.buildDir.toString))
      if(buildEnv.buildDir.exists) {
        Files.deleteDir(buildEnv.buildDir)
      }
      buildEnv.buildDir.mkdirs

      val gimmeCmds = Seq(Gimme.getCommand(dirs.workflowBaseDir, packageName))
      val env = Seq()
      val exitCode = Shell.run(gimmeCmds, buildEnv.buildDir, env, buildEnv.buildStdoutFile, buildEnv.buildStderrFile)
      if(exitCode != 0) {
        println("%sBuild task %s returned %s%s".format(conf.errorColor, packageName, exitCode, Console.RESET))
        System.exit(1)
      }

    }
  }
}