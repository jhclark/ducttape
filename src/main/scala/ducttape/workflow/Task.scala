// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

// TODO: Rename
object Task {
  val NO_BRANCH_POINT = new BranchPoint { val name = "Baseline" }
  val NO_BRANCH = new Branch {
    val name = "baseline"
    val baseline = true
    val branchPoint = NO_BRANCH_POINT
  }
  val NO_REALIZATION = new Realization(Seq(NO_BRANCH))
}