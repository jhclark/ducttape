package ducttape.workflow

// TODO: Rename
object Task {
  val NO_BRANCH_POINT = new BranchPoint { val name = "Baseline" }
  val NO_BRANCH = new Branch {
    val name = "baseline"
    val baseline = true
    val branchPoint = NO_BRANCH_POINT
  }
  val NO_REALIZATION = new Realization(Seq(NO_BRANCH))
}