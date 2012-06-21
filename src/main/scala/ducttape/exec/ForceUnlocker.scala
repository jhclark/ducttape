package ducttape.exec

import java.io.File
import collection._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.workflow.Branch
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.util.Environment
import ducttape.versioner.WorkflowVersionInfo

// see also LockManager
class ForceUnlocker(dirs: DirectoryArchitect, todo: Set[(String,Realization)]) extends UnpackedDagVisitor {

  override def visit(task: RealTask) {
    if (todo( (task.name, task.realization) )) {
      val taskEnv = new TaskEnvironment(dirs, task)
      LockManager.forceReleaseLock(taskEnv)
    }
  }
}
