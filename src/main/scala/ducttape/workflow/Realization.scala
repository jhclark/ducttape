package ducttape.workflow

import collection._

// TODO: Move into HyperDAG?
class Realization(val branches: Seq[Branch]) {
 // TODO: Keep string branch point names?

  // sort by branch *point* names to keep ordering consistent, then join branch names using dashes
  // and don't include our default branch "baseline"
  private def realizationName(real: Map[String,Branch]): String = {
    val branches = real.toSeq.sortBy(_._1).map(_._2)
    val names = branches.map(_.name).filter(_ != Task.NO_BRANCH.name)
    // TODO: Can we get rid of some of these cases now?
    names.size match {
      case 0 => Task.NO_BRANCH.name // make sure we have at least baseline in the name
      case _ => names.mkString("-")
    }
  }

  //def realizationName(real: Seq[Branch]) = realizationName(branchesToMap(real))
  private def branchesToMap(real: Seq[Branch]) = {
    val result = new mutable.HashMap[String,Branch]
    result += Task.NO_BRANCH_POINT.name -> Task.NO_BRANCH // TODO: XXX: Should we enforce this elsewhere?
    for(branch <- real) {
      result += branch.branchPoint.name -> branch
    }
    result
  }

  lazy val activeBranchMap = branchesToMap(branches)
  lazy val str = realizationName(activeBranchMap)

  override def hashCode = str.hashCode // TODO: More efficient?
  override def equals(obj: Any) = obj match { case that: Realization => this.str == that.str } // TODO: More efficient?
  override def toString = str
}