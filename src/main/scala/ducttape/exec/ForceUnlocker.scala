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
class ForceUnlocker(dirs: DirectoryArchitect, todo: Set[(String,Realization)]) extends UnpackedDagVisitor {

  override def visit(task: VersionedTask) {
    if (todo( (task.name, task.realization) )) {
      val taskEnv = new TaskEnvironment(dirs, task)
      LockManager.forceReleaseLock(taskEnv)
    }
  }
}
