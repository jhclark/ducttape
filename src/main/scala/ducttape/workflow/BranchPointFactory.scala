package ducttape.workflow

import collection._

class NoSuchBranchPointException(val msg: String) extends Exception(msg)

// pool branch points to make comparison easier
class BranchPointFactory {
  private val pool = new mutable.HashMap[String,BranchPoint]
  pool += Task.NO_BRANCH_POINT.name -> Task.NO_BRANCH_POINT

  // creates a new BranchPoint if one isn't found
  private[workflow] def get(bpName: String): BranchPoint = {
    pool.getOrElseUpdate(bpName, new BranchPoint {
      override val name = bpName
    })
  }
  
  // throws if branch point name isn't found
  def apply(name: String): BranchPoint = try {
    pool(name)
  } catch {
    case e: NoSuchElementException => throw new NoSuchBranchPointException(name)
  }
}


