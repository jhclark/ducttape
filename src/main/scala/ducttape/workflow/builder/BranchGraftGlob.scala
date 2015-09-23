// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow.builder

import ducttape.syntax.AbstractSyntaxTree.BranchGraftElement
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPointFactory

object BranchGraftGlob {
  
  def expand(
      branchGraftElements: Seq[BranchGraftElement],
      branchPointFactory: BranchPointFactory, 
      branchFactory: BranchFactory,
      accumulator: Seq[BranchGraftElement] = Seq.empty)
    : Seq[Seq[BranchGraftElement]] = {
        
    branchGraftElements.headOption match {
      case None                     => return Seq(accumulator)
      case Some(branchGraftElement) => {
        if (branchGraftElement.isGlob) {
          val branches = branchFactory.getAll(branchGraftElement.branchPointName).toSeq
          return branches.flatMap( (branch: Branch) => {
            val expandedBranchGraftElement = new BranchGraftElement(branchGraftElement.branchPointName, branch.name)
            expand(branchGraftElements.tail, branchPointFactory, branchFactory, accumulator++Seq(expandedBranchGraftElement)) 
          })
        } else {
          return expand(branchGraftElements.tail, branchPointFactory, branchFactory, accumulator++Seq(branchGraftElement))
        }
      }
    }
    
  }  
  
}