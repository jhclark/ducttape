package ducttape.workflow

import ducttape.syntax.Namespace

// holds the information necessary to uniquely identify a task's directory
// minus the version
//
// "realization" should be the *canonical* string representation
// as we might need to compare against realization strings from the version history
// We don't store realization as the proper Realization type since parsing a realization
// may result in BranchPoints or Branches that no longer exist in the current version's AST.
// (and are therefore invalid under the current definition of those types). 
//
// (see also docs for VersionedTaskId)
class RealTaskId(val name: Namespace, val realization: String) {

  /** constructor used by InPlanConstraint -- needs to be fast */
  def this(name: Namespace, branches: Seq[Branch])
    = this(name, new Realization(branches).toCanonicalString)

  def toVersionedTaskId(version: Int) = new VersionedTaskId(name, realization, version)

  // TODO: Smear hash code better
  override def hashCode() = name.hashCode ^ realization.hashCode
  override def equals(obj: Any) = obj match {
    case that: RealTaskId => {
      // use hash code to weed out non-matches faster
      this.name == that.name &&
      this.realization.hashCode == that.realization.hashCode &&
      this.realization == that.realization
    }
  }
  override def toString(): String = s"${name}/${realization}"
}
