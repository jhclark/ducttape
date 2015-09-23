// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow.builder

import ducttape.workflow.Branch
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.SpecTypes.SpecPair
import scala.collection.mutable

/**
 * TODO: Explain to Lane what this class is
 */
private[builder] class TerminalData(
    val task: Option[TaskDef],
    val grafts: Seq[Branch],
    val isParam: Boolean) {

  val specs = new mutable.ArrayBuffer[SpecPair]

      override def toString() = "(%s %s isParam=%s %s)".format(task, grafts, isParam, specs)
}
