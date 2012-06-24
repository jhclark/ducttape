package ducttape.workflow

import collection._

class NoSuchBranchPointException(val msg: String) extends Exception(msg)

/**
 * Workflow-specific factory for creating <code>BranchPoint</code> objects.
 * <p>
 * This class maintains a pool of named branch points.
 * The pool is not shared between different workflows, 
 * and as such this factory is a class rather than an object. 
 */
class BranchPointFactory {

  /** Pool branch points to make comparison easier */
  private val pool = new mutable.HashMap[String,BranchPoint]
  pool += Task.NO_BRANCH_POINT.name -> Task.NO_BRANCH_POINT

  /**
   * Gets the named branch point from the pool of existing branch point objects, 
   * or creates a new branch point if it isn't found in the pool.
   * 
   * @param bpName Name of the branch point to be retrieved
   */
  private[workflow] def get(bpName: String): BranchPoint = {
    pool.getOrElseUpdate(bpName, new BranchPoint {
      override val name = bpName
    })
  }
  
  /**
   * Gets the named branch point from the pool of existing branch point objects, 
   * throwing an exception if it isn't found in the pool.
   * 
   * @param name Name of the branch point to be retrieved
   * @throws NoSuchElementException
   */  
  def apply(name: String): BranchPoint = try {
    pool(name)
  } catch {
    case e: NoSuchElementException => throw new NoSuchBranchPointException(name)
  }
}
