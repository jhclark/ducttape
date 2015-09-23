// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

/**
 * Interface for a workflow branch point.
 * <p>
 * A branch point has a name, and within a workflow will be associated with one or more branches. 
 */
trait BranchPoint {
  
  /** Name of the branch point. */
  val name: String
  
  override def hashCode() = name.hashCode
  
  // TODO: Take advantage of pooling
  override def equals(obj: Any) = obj match { case that: BranchPoint => this.name == that.name }
  
  override def toString() = name
  
}