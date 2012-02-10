package ducttape.workflow

import collection._

// TODO: Separate package?

trait BranchPoint {
  def name: String
  override def hashCode = name.hashCode
  // TODO: Take advantage of pooling
  override def equals(obj: Any) = obj match { case that: BranchPoint => this.name == that.name }
  override def toString = name
}

trait Branch {
  def name: String
  def branchPoint: BranchPoint
  override def hashCode = name.hashCode
  override def equals(obj: Any) = obj match { // TODO: Take advantage of pooling
    case that: Branch => this.name == that.name && this.branchPoint == that.branchPoint
  }
  override def toString = name + "@" + branchPoint
}

// TODO: Rename
object Task {
  val NO_BRANCH_POINT = new BranchPoint { val name = "Baseline" }
  val NO_BRANCH = new Branch {
    val name = "baseline"
    val branchPoint = NO_BRANCH_POINT
  }
}

class NoSuchBranchPointException(val msg: String) extends Exception(msg)
class NoSuchBranchException(val msg: String) extends Exception(msg)

// pool branch points to make comparison easier
class BranchPointFactory {
  private val pool = new mutable.HashMap[String,BranchPoint]
  pool += Task.NO_BRANCH_POINT.name -> Task.NO_BRANCH_POINT

  // creates a new BranchPoint if one isn't found
  private[workflow] def get(bpName: String): BranchPoint = {
    pool.getOrElseUpdate(bpName, new BranchPoint { val name=bpName })
  }
  
  // throws if branch point name isn't found
  def apply(name: String): BranchPoint = try {
    pool(name)
  } catch {
    case e: NoSuchElementException => throw new NoSuchBranchPointException(name)
  }
}

// pool branch points to make comparison easier
class BranchFactory(bpf: BranchPointFactory) {
  private val pool = new mutable.HashMap[(String,BranchPoint),Branch]
  pool += (Task.NO_BRANCH.name, Task.NO_BRANCH_POINT) -> Task.NO_BRANCH

  // creates new if branch isn't found
  private[workflow] def get(myName: String, myBranchPoint: BranchPoint): Branch = {
    pool.getOrElseUpdate( (myName, myBranchPoint), new Branch {
      val name = myName
      val branchPoint = myBranchPoint
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
