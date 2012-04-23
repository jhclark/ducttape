package ducttape.workflow

trait BranchPoint {
  val name: String
  override def hashCode() = name.hashCode
  // TODO: Take advantage of pooling
  override def equals(obj: Any) = obj match { case that: BranchPoint => this.name == that.name }
  override def toString() = name
}