// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

object RealizationOrdering extends Ordering[Branch] {
  private val delegate = Ordering.fromLessThan {
    (a: Branch, b: Branch) => a.branchPoint.name < b.branchPoint.name
  }
  override def compare(x: Branch, y: Branch) = delegate.compare(x, y)
}