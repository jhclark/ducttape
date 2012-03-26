package ducttape.workflow

trait Branch {
  def name: String
  def branchPoint: BranchPoint
  override def hashCode = name.hashCode
  override def equals(obj: Any) = obj match { // TODO: Take advantage of pooling
    case that: Branch => this.name == that.name && this.branchPoint == that.branchPoint
  }
  override def toString = name + "@" + branchPoint
}