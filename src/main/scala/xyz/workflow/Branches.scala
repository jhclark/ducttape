// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package xyz.workflow

class Branch(val branchPoint: BranchPoint, val name: String) {

  override def toString() = "%s.%s".format(branchPoint.name,name)

}


class BranchPoint(val name: String, branchNames:Set[String]) {

  val branches:Seq[Branch] = branchNames.toArray.sorted.map { branchName => new Branch(this, branchName) }

  override def toString() = "(%s: %s)".format(name, branches.map({ branch => branch.name }).mkString(" "))

}


