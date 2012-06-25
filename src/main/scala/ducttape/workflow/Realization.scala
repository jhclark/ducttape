package ducttape.workflow

import collection._

// Note: branches are now guaranteed to be sorted by the HyperDAG
// see RealizationOrdering
class Realization(val branches: Seq[Branch]) {
 // TODO: Keep string branch point names?

  // sort by branch *point* names to keep ordering consistent, then join branch names using dashes
  //   and don't include our default branch "baseline"
  // by default we "shorten" realization names by removing branch points using baseline branches
  //   such that new branch points can easily be added since the entire directory structure doesn't
  //   suddenly change
  private def realizationName(): String = {
    
    // sort by branch point name and remove references to the baseline branch
    // (removing all instances of baseline for any branch allows easier extensibility)
    val filteredBranches: Seq[Branch] = branches.filter { !_.baseline } match {
      // make sure we have at least baseline, if nothing else
      case Seq() => Seq(Task.NO_BRANCH)
      case myBranches => myBranches 
    }
                                     
    // write baseline branch to disk as "baseline" to allow for easier extensibility
    // while maintaining semantics
    val names = filteredBranches.map { branch =>
      val displayName = if (branch.baseline) "baseline" else branch.name
      "%s.%s".format(branch.branchPoint.name, displayName)
    }
    names.mkString("-")
  }
    
  private def fullRealizationName(): String = {
    // TODO: Prohibit the branch point name "Baseline"?
    val filteredBranches: Seq[Branch] = branches.filter { _.branchPoint != Task.NO_BRANCH_POINT }
    val names = filteredBranches.map { branch => "%s.%s".format(branch.branchPoint.name, branch.name) }
    names.mkString("-")
  }

  lazy val str = realizationName()

  override def hashCode() = str.hashCode // TODO: More efficient?
  override def equals(obj: Any) = obj match { case that: Realization => this.str == that.str } // TODO: More efficient?
  override def toString() = str
  
  // unshortened realization name
  def toFullString(): String = fullRealizationName()
}
