package ducttape.workflow

import HyperWorkflow._
import ducttape.workflow.SpecTypes.SpecPair
import ducttape.workflow.Types.UnpackState
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.Types.WorkflowEdge
import ducttape.workflow.Types.PackedWorkVert
import ducttape.hyperdag.meta.PhantomMetaHyperDag

import grizzled.slf4j.Logging

 /** when used with an unpacker, causes hyperedge grafts to be recognized
 *  and handled properly
 *  
 *  The explainCallback can be used to provide feedback to the user on
 *  why certain realizations were not produced (e.g. due to grafting). */
class BranchGraftMunger(
      dag: PhantomMetaHyperDag[TaskTemplate,BranchPoint,BranchInfo,Seq[SpecPair]],
      explainCallback: ExplainCallback)
    extends HyperWorkflowStateMunger with Logging {
  
  override def traverseEdge(v: PackedWorkVert,
      heOpt: Option[WorkflowEdge],
      e: Seq[SpecPair],
      parentRealization: Seq[Branch],
      prevState: UnpackState): Option[UnpackState] = {
    
    heOpt match {
      case Some(he) => {
        trace ("Considering if we need to apply a graft for he '%s' with sink '%s': ".format(he, v, prevState))
        if (he.h == null || he.h.grafts.size == 0) { // TODO: Why is this null check necessary?
          // no grafting required. do nothing
          trace("No grafting required")
          Some(prevState)
        } else {
          // assert that the current state contains all of the branches specified by the graft
          if (he.h.grafts.forall { branch => prevState.get(branch.branchPoint) == Some(branch) } ) {
            // now that we've asserted that the grafts, remove all grafted branches from the state
            val nextState = he.h.grafts.foldLeft(prevState) { case (state, branch) =>
              state - branch.branchPoint
            }
            trace("Applied grafts: %s => %s".format(prevState.values, nextState.values))
            Some(nextState)
          } else {
            trace("Filtered by branch graft")
            // no corresponding edge was found in the derivation
            // this branch graft cannot apply
            explainCallback(
              v.toString,
              "Realization %s filtered by branch graft: %s (source: %s)".format(
                prevState.values.mkString("-"),
                he.h.grafts.mkString(","),
                heOpt.map { he => dag.delegate.delegate.sources(he).head.toString }.getOrElse("unknown")),
              false)
            None
          }
        }
      }
      case _ => Some(prevState)
    }
  }
}