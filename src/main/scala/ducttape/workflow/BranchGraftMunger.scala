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
 *  Note that some grafts are enforced statically at compile time.
 *  This is true for 1) grafts on parameters (since parameters don't imply
 *  temporal dependencies, they are only linked to phantom vertices in
 *  the HyperDAG) and 2) grafts within nested branch point definitions.
 *  
 *  The explainCallback can be used to provide feedback to the user on
 *  why certain realizations were not produced (e.g. due to grafting). */
class BranchGraftMunger(
      dag: PhantomMetaHyperDag[TaskTemplate,BranchPoint,Branch,SpecGroup],
      explainCallback: ExplainCallback)
    extends HyperWorkflowStateMunger with Logging {
  
  override def traverseEdge(v: PackedWorkVert,
      heOpt: Option[WorkflowEdge],
      e: SpecGroup,
      parentReal: Seq[Branch],
      prevState: UnpackState): Option[UnpackState] = {
    
    heOpt match {
      case Some(he) => {
        trace ("Considering if we need to apply a graft for he '%s' with sink '%s': ".format(he, v, prevState))
        if (e == null || e.grafts.size == 0) {
          // no grafting required. do nothing
          trace("No grafting required")
          Some(prevState)
        } else {
          // assert that the current state contains all of the branches specified by the graft
          if (e.grafts.forall { branch => prevState.edgeState.get(branch.branchPoint) == Some(branch) } ) {
            // now that we've asserted that the grafts, remove all grafted branches from the state
            val nextState = e.grafts.foldLeft(prevState.edgeState) { case (state, branch) =>
              state - branch.branchPoint
            }
            trace("Applied grafts: %s => %s".format(prevState.edgeState.values, nextState.values))
            Some(new UnpackState(hyperedgeState=prevState.hyperedgeState, edgeState=nextState))
          } else {
            trace("Filtered by branch graft")
            // no corresponding edge was found in the derivation
            // this branch graft cannot apply
            explainCallback(
              v.toString,
              "Realization %s filtered by branch graft: %s (source: %s)".format(
                prevState,
                e.grafts.mkString(","),
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
