// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

import ducttape.workflow.HyperWorkflow.HyperWorkflowStateMunger
import ducttape.workflow.SpecTypes.SpecPair
import ducttape.workflow.Types.UnpackState
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.Types.WorkflowEdge
import ducttape.workflow.Types.PackedWorkVert
import ducttape.hyperdag.meta.PhantomMetaHyperDag
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.HyperEdge

import grizzled.slf4j.Logging

object GlobalBranchPointConstraint extends HyperWorkflowStateMunger with Logging {

  // v: the sink vertex of this active hyperedge      
  // parentReal: the realization at the parent, which we are proposing to add (traverse)
  override def traverseEdge(v: PackedVertex[Option[TaskTemplate]],
                            heOpt: Option[HyperEdge[Branch,SpecGroup]],
                            e: SpecGroup,
                            parentReal: Seq[Branch],
                            prevState: UnpackState): Option[UnpackState] = {
    
    assert(prevState != null)
    assert(parentReal != null)
    assert(!parentReal.exists(_ == null))
    
    trace("Applying globalBranchPointConstraint at %s with hyperedge %s for realization: %s".format(v, heOpt, prevState))
    
    // enforce that each branch point should atomically select one branch per hyperpath
    // through the (Meta)HyperDAG
    def violatesChosenBranch(chosenBranches: Map[BranchPoint,Branch], proposedBranch: Branch) = {
      chosenBranches.get(proposedBranch.branchPoint) match {
        case None => {
          trace("No branch chosen yet for: " + proposedBranch.branchPoint)
          false // no branch chosen yet
        }
        case Some(prevChosenBranch) => {
          trace("Enforcing constraint: %s == %s or bust".format(proposedBranch, prevChosenBranch))
          proposedBranch != prevChosenBranch
        }
      }
    }

    val proposedBranches = prevState.edgeState.values
    val chosenBranches = prevState.hyperedgeState
    
    if (proposedBranches.exists { branch => violatesChosenBranch(chosenBranches, branch) } ) {
      debug("Vertex=%s; e=%s; Discarded globally inconsistent realization: %s".format(v, e, prevState))
      None // we've already seen this branch point before -- and we just chose the wrong branch
    } else {
      Some(prevState)
    }
  }
}