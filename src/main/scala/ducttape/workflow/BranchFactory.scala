package ducttape.workflow

import collection._

class NoSuchBranchException(val msg: String) extends Exception(msg)

// pool branches to make comparison easier
class BranchFactory(bpf: BranchPointFactory) {
  private val pool = new mutable.HashMap[(String,BranchPoint),Branch]
  pool += (Task.NO_BRANCH.name, Task.NO_BRANCH_POINT) -> Task.NO_BRANCH

  // creates new if branch isn't found
  private[workflow] def get(myName: String, myBranchPoint: BranchPoint): Branch = {
    pool.getOrElseUpdate( (myName, myBranchPoint), new Branch {
      override val name = myName
      override val branchPoint = myBranchPoint
    } )
  }
  // creates new if branch isn't found
  private[workflow] def get(name: String, branchPoint: String): Branch = {
    get(name, bpf.get(branchPoint))
  }
  
  // throws if branch isn't found
  def apply(name: String, branchPoint: BranchPoint): Branch = try {
    pool( (name, branchPoint) )
  } catch {
    case e: NoSuchElementException => throw new NoSuchBranchException("%s@%s".format(name, branchPoint))
  }
  
  // throws if branch isn't found
  def apply(name: String, branchPoint: String): Branch = {
    apply(name, bpf(branchPoint))
  }
}