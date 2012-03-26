package ducttape.exec
import java.io.File

class BuildEnvironment(val dirs: DirectoryArchitect, val workflowVersion: Int, val packageName: String) {
  val buildDir = new File(dirs.assignBuildDir(packageName), workflowVersion.toString)
  val buildStdoutFile = new File(buildDir, "gimme_stdout.txt")
  val buildStderrFile = new File(buildDir, "gimme_stderr.txt")  
}