// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import ducttape.workflow.TaskTemplate
import ducttape.util.Files

class Purger(dirs: DirectoryArchitect) extends PackedDagVisitor {
  override def visit(task: TaskTemplate) {
    val where = dirs.assignPackedDir(task.taskDef.name)
    println("Removing directory: %s".format(where.getAbsolutePath))
    if (where.exists) {
      Files.deleteDir(where)
    }
  }
}
