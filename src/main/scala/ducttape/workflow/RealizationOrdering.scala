package ducttape.workflow

object RealizationOrdering extends Ordering[Branch] {
  private val delegate = Ordering.fromLessThan {
    (a: Branch, b: Branch) => a.branchPoint.name < b.branchPoint.name
  }
  override def compare(x: Branch, y: Branch) = delegate.compare(x, y)
}