// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import ducttape.workflow.RealTask
import ducttape.workflow.VersionedTask

// TODO: Move this to its own file
trait UnpackedRealDagVisitor {
  def visit(task: RealTask)
}

// TODO: Add Versioned to name
trait UnpackedDagVisitor {
  def visit(task: VersionedTask)
}
