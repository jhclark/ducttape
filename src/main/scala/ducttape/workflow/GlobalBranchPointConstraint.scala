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
  // real: the current realization of this vertex
  // seen: the non-local derivation state we pass around (more efficient to access than real)
  // parentReal: the realization at the parent, which we are proposing to add (traverse)
  override def traverseEdge(v: PackedVertex[Option[TaskTemplate]],
                            heOpt: Option[HyperEdge[BranchInfo,Seq[SpecPair]]],
                            e: Seq[SpecPair],
                            parentReal: Seq[Branch],
                            prevState: UnpackState): Option[UnpackState] = {
    
    assert(prevState != null)
    assert(parentReal != null)
    assert(!parentReal.exists(_ == null))
    
    trace("Applying globalBranchPointConstraint at %s with hyperedge %s for realization: %s".format(v, heOpt, prevState.values.mkString("-")))
    
    // enforce that each branch point should atomically select one branch per hyperpath
    // through the (Meta)HyperDAG
    def violatesChosenBranch(seen: UnpackState, newBranch: Branch) = seen.get(newBranch.branchPoint) match {
      case None => {
        trace("No branch chosen yet for: " + newBranch.branchPoint)
        false // no branch chosen yet
      }
      case Some(prevChosenBranch) => {
        trace("Enforcing constraint: %s == %s or bust".format(newBranch, prevChosenBranch))
        newBranch != prevChosenBranch
      }
    }

    if (parentReal.exists { branch => violatesChosenBranch(prevState, branch) } ) {
      None // we've already seen this branch point before -- and we just chose the wrong branch
    } else {
      // left operand determines return type (an efficient immutable.HashMap)
      val result: UnpackState = prevState ++ parentReal.map { b: Branch => (b.branchPoint, b) }
      trace("Extending seen at " + v + ": " + prevState.values + " with " + parentReal + " ==> " + result.values)
      Some(result)
    }
  }
}