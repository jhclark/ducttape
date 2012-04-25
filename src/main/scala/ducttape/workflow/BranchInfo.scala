package ducttape.workflow

/**
 * Denotes a branch and any branch grafts associated with that branch.
 * A BranchInfo instance is typically associated with a list of Specs.
 */
class BranchInfo(val branch: Branch, val grafts: Seq[Branch]) {
  // TODO: Smear
  override def hashCode() = branch.hashCode ^ grafts.hashCode
  override def equals(obj: Any) = obj match { // TODO: Take advantage of pooling
    case that: BranchInfo => this.branch == that.branch && this.grafts == that.grafts
  }
  override def toString() = "%s[%s]".format(branch, grafts.mkString(","))
}