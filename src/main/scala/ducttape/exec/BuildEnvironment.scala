package ducttape.exec
import java.io.File

class BuildEnvironment(val dirs: DirectoryArchitect, val packageVersion: String, val packageName: String) {
  val buildDir = dirs.assignBuildDir(packageName, packageVersion)
  val buildStdoutFile = new File(buildDir, "build_stdout.txt")
  val buildStderrFile = new File(buildDir, "build_stderr.txt")  
}