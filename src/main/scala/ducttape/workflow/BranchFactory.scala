// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

import collection._
import grizzled.slf4j.Logging

class NoSuchBranchException(val msg: String) extends Exception(msg)

// pool branches to make comparison easier
class BranchFactory(bpf: BranchPointFactory) extends Logging {
  
  private val pool = new mutable.HashMap[(String,BranchPoint),Branch]
  pool += (Task.NO_BRANCH.name, Task.NO_BRANCH_POINT) -> Task.NO_BRANCH

  // creates new if branch isn't found
  private[workflow] def get(myName: String, myBranchPoint: BranchPoint, isBaseline: Boolean): Branch = {
    assert(myName != null)
    if (isBaseline) debug("New baseline branch: %s:%s".format(myBranchPoint, myName))
    pool.getOrElseUpdate( (myName, myBranchPoint), new Branch {
      override val name = myName
      override val baseline = isBaseline
      override val branchPoint = myBranchPoint
    } )
  }
  // creates new if branch isn't found
  private[workflow] def get(name: String, branchPoint: String, isBaseline: Boolean): Branch = {
    assert(name != null)
    get(name, bpf.get(branchPoint), isBaseline)
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
  
  /**
   * Gets a map containing all branches for each branch point
   */
  def getAll() : Map[BranchPoint,Iterable[Branch]] = { 
    return pool.values.groupBy{ branch: Branch => branch.branchPoint }
  }
  
  /**
   * Gets all branches for a particular branch point
   */
  def getAll(branchPointName: String) : Iterable[Branch] = { 
    val branchPoint = bpf.get(branchPointName)
    val map = getAll()
    return map(branchPoint)
  }
  
}
