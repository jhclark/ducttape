package ducttape.workflow.builder

import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.Branch
import ducttape.workflow.BranchPoint
import scala.collection.mutable

/**
 * See BranchPointTree
 * 
 * branch will be baseline for the root vertex
 */
private[builder] class BranchTree(val branch: Branch) {

  // if populated at the root, indicates no branch points
  //   specs, organized by which task they originate from
  //   and then by what grafts apply for that parent
  // NOTE: These are the final resolved specs, appropriate for use within some realization
  //   they are used in the BranchTreeMap; the plain edges in the HyperDAG
  //   are populated by the *original* unresolved specs
  // In the end, each element of terminalData will become an edge
  //   within some hyperedge
  val terminalData = new mutable.ArrayBuffer[TerminalData]
  val children     = new mutable.ArrayBuffer[BranchPointTreeData]

  def getOrAdd(bp: BranchPoint, grafts: Seq[Branch]): BranchPointTreeData = {
      children.find { child => child.tree.branchPoint == bp && child.grafts == grafts } match {
      case Some(found) => found
      case None => {
        val bpt = new BranchPointTree(bp)
        val child = new BranchPointTreeData(bpt, grafts)
        children += child
        child
      }
      }
  }

  def getOrAdd(task: Option[TaskDef], grafts: Seq[Branch], isParam: Boolean): TerminalData = {
      // TODO: Do we need to sort grafts so that we don't get multiple entries
      // if they're in different orders?
      terminalData.find { data => data.task == task && data.grafts == grafts && data.isParam == isParam } match {
      case Some(data) => data
      case None => {
        val result = new TerminalData(task, grafts, isParam)
        terminalData += result
        result
      }
      }
  }

  override def toString() = "(B=" + branch + " :: terminalData=" + terminalData.mkString(":") + " :: " + children + ")"
}
