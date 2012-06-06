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
class PidWriter(dirs: DirectoryArchitect,
                version: WorkflowVersionInfo,
                todo: Set[(String,Realization)],
                remove: Boolean = false) extends UnpackedDagVisitor {
  val locker = new LockManager(version)

  override def visit(task: RealTask) {
    if (todo( (task.name, task.realization) )) {
      val taskEnv = new TaskEnvironment(dirs, task)
      if (!remove) {
        // TODO: Only write the lock if someone else doesn't own it!
        locker.writeLock(taskEnv)
      } else {
        // TODO: Only release the lock if we own it!
        locker.releaseLock(taskEnv)
      }
    }
  }
}
