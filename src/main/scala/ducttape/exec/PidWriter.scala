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
        // NOTE: We can never end up deadlocked,
        // since the original workflow will eventually terminate,
        // which implies that the next workflow will eventually begin running
        //
        // We must be clear about the semantics of what happens when a task
        // is locked when a workflow is launched:
        // 1) if the task completes successfully, the previous version will be used
        // 2) if the task fails, the first workflow who acquires the lock
        //    will run it (WARNING: it's the user's responsibility to
        //    make sure all currently running versions will provide
        //    *consistent* output)
        locker.maybeAcquireLock(taskEnv)
      } else {
        locker.maybeReleaseLock(taskEnv)
      }
    }
  }
}
