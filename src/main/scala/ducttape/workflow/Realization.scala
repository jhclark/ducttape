package ducttape.workflow

import collection._

// TODO: Move into HyperDAG?
class Realization(val branches: Seq[Branch]) {
 // TODO: Keep string branch point names?

  // sort by branch *point* names to keep ordering consistent, then join branch names using dashes
  // and don't include our default branch "baseline"
  private def realizationName(real: Map[String,Branch]): String = {
    
    // sort by branch point name and remove references to the baseline branch
    // (removing all instances of baseline for any branch allows easier extensibility)
    val branches: Seq[Branch] = real.values.toSeq.
                                     sortBy(_.branchPoint.name).
                                     filterNot { _.baseline } match {
                                       // make sure we have at least baseline, if nothing else
                                       case Seq() => Seq(Task.NO_BRANCH)
                                       case myBranches => myBranches 
                                     }
                                     
    // write baseline branch to disk as "baseline" to allow for easier extensibility
    // while maintaining semantics
    val names = branches.map { branch =>
      val displayName = if (branch.baseline) "baseline" else branch.name
      "%s.%s".format(branch.branchPoint.name, displayName)
    }
    names.mkString("-")
  }

  //def realizationName(real: Seq[Branch]) = realizationName(branchesToMap(real))
  private def branchesToMap(real: Seq[Branch]) = {
    val result = new mutable.HashMap[String,Branch]
    result += Task.NO_BRANCH_POINT.name -> Task.NO_BRANCH // TODO: XXX: Should we enforce this elsewhere?
    for (branch <- real) {
      result += branch.branchPoint.name -> branch
    }
    result
  }

  lazy val activeBranchMap = branchesToMap(branches)
  lazy val str = realizationName(activeBranchMap)

  override def hashCode() = str.hashCode // TODO: More efficient?
  override def equals(obj: Any) = obj match { case that: Realization => this.str == that.str } // TODO: More efficient?
  override def toString() = str
}