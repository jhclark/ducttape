package ducttape.workflow

// TODO: Rename
object Task {
  val NO_BRANCH_POINT = new BranchPoint { val name = "Baseline" }
  val NO_BRANCH = new Branch {
    val name = "baseline"
    val branchPoint = NO_BRANCH_POINT
  }
}