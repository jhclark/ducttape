package ducttape.exec

import java.io.File
import collection._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.workflow.Branch
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.util.Files
import ducttape.util.Environment
import ducttape.versioner.WorkflowVersionInfo

class PidWriter(dirs: DirectoryArchitect,
                version: WorkflowVersionInfo,
                todo: Set[(String,Realization)],
                remove: Boolean = false) extends UnpackedDagVisitor {

  override def visit(task: RealTask) {
    if (todo( (task.name, task.realization) )) {
      val taskEnv = new TaskEnvironment(dirs, task)
      if (!remove) {
        Files.write("%s:%d".format(version.hostname, version.pid), taskEnv.lockFile)
        Files.write(version.version.toString, taskEnv.versionFile)
      } else {
        taskEnv.lockFile.delete()
      }
    }
  }
}
