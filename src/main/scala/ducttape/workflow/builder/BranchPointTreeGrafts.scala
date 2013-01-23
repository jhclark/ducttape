package ducttape.workflow.builder

import ducttape.workflow.Branch

/**
 * TODO: Explain to Lane what this class is
 */
private[builder] class BranchPointTreeGrafts(
    val tree: BranchPointTree,
    val grafts: Seq[Branch]) {
  override def toString() = tree + "+grafts=[" + grafts.mkString(",") + "]"
}
