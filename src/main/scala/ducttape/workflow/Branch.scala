// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

trait Branch {
  val name: String
  val baseline: Boolean
  val branchPoint: BranchPoint
  
  override def hashCode() = name.hashCode
  override def equals(obj: Any) = obj match { // TODO: Take advantage of pooling
    case that: Branch => {
      (this.baseline && that.baseline || this.name == that.name) && this.branchPoint == that.branchPoint
    }
  }
  override def toString() = s"${branchPoint.name}.${name}"
}
