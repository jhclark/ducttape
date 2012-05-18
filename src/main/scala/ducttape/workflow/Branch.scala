package ducttape.workflow

trait Branch {
  val name: String
  val branchPoint: BranchPoint
  override def hashCode() = name.hashCode
  override def equals(obj: Any) = obj match { // TODO: Take advantage of pooling
    case that: Branch => this.name == that.name && this.branchPoint == that.branchPoint
  }
  override def toString() = "%s:%s".format(branchPoint.name, name)
}
