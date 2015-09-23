// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow.builder

import ducttape.workflow.Branch
import ducttape.workflow.BranchPoint
import ducttape.workflow.SpecTypes.SpecPair
import scala.collection.mutable

/** Stores information from TaskTemplateBuilder to be passed back to WorflowBuilder
 *  so that we know which specs and grafts should be used given some realization.
 *  This small tree snippet is rooted at a single task and records the inputs and parameters for
 *  that task. It is then used to create the full HyperDAG structure
 *  by the WorkflowBuilder in the traverse() and getHyperedges() methods.
 * 
 *  alternates with BranchInfoTree */
private[builder] class BranchPointTree(val branchPoint: BranchPoint) {
  val children = new mutable.ArrayBuffer[BranchTree]

  def getOrAdd(br: Branch): BranchTree = 
    children.find { child: BranchTree => 
      (child.branch == br) 
    } match {
      case Some(found) => found
      case None => {
        val child = new BranchTree(br)
        children += child
        child
      }
    }

  // recursively enumerate all specs in this tree
  def specs: Iterable[SpecPair] = children.flatMap { child: BranchTree =>
    child.terminalData.flatMap { data: TerminalData => data.specs } ++
    child.children.flatMap { grandchild: BranchPointTreeGrafts => grandchild.tree.specs }
  }

  override def toString() = "(BP=" + branchPoint + ": " + children.mkString + ")"
}
