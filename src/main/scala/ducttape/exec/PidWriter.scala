// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import java.io.File
import collection._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.workflow.Branch
import ducttape.workflow.Realization
import ducttape.workflow.VersionedTask
import ducttape.util.Environment
import ducttape.versioner.WorkflowVersionInfo

// see also LockManager
class PidWriter(dirs: DirectoryArchitect,
                todo: Set[(String,Realization)],
                locker: LockManager,
                remove: Boolean = false) extends UnpackedDagVisitor {

  override def visit(task: VersionedTask) {
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
        locker.maybeAcquireLock(taskEnv, writeVersion=true)
      } else {
        locker.maybeReleaseLock(taskEnv)
      }
    }
  }
}
