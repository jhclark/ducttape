package ducttape.workflow

import ducttape.util.HashUtils

import collection._

import grizzled.slf4j.Logging

// Note: branches are now guaranteed to be sorted by the HyperDAG
// see RealizationOrdering
class Realization(val branches: Seq[Branch]) extends Logging {
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
    val result = names.mkString(Realization.delimiter)
    if (result.length > 255) {
      throw new RuntimeException("Got realization name longer than 255 characters. This might cause issues on disk: %s".format(result))
    }
    result
  }
    
  private def fullRealizationName(hashLongNames: Boolean = true): String = {
    // TODO: Prohibit the branch point name "Baseline"?
    val filteredBranches: Seq[Branch] = branches.filter { _.branchPoint != Task.NO_BRANCH_POINT }
    val names = filteredBranches.map { branch => "%s.%s".format(branch.branchPoint.name, branch.name) }
    val result = names.mkString(Realization.delimiter)
    if (result.length > 255 && hashLongNames) {
      warn("Very long filename is being hashed: " + result)
      HashUtils.md5(result)
      //throw new RuntimeException("Got realization name longer than 255 characters. This might cause issues on disk: %s".format(result))
    } else {
      result
    }
  }

  lazy val str = realizationName()

  override def hashCode() = str.hashCode // TODO: More efficient?
  override def equals(obj: Any) = obj match { case that: Realization => this.str == that.str } // TODO: More efficient?
  override def toString() = str
  
  // unshortened realization name
  def toFullString(hashLongNames: Boolean = true): String = fullRealizationName(hashLongNames)
  
  // returns true if this realization has only one branch and it is Baseline.baseline
  def hasSingleBranchBaseline(): Boolean = {
    branches == Seq(Task.NO_BRANCH) || branches.isEmpty
  }
}

object Realization {
  var delimiter: String = "+"
}
