package ducttape.workflow

import HyperWorkflow._
import ducttape.workflow.SpecTypes.SpecPair
import ducttape.workflow.Types.UnpackState
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.Types.WorkflowEdge
import ducttape.workflow.Types.PackedWorkVert
import ducttape.hyperdag.meta.PhantomMetaHyperDag
import grizzled.slf4j.Logging
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.HyperEdge

// adds the proposed realization to the edgeState
object EdgeStateInitializer extends HyperWorkflowStateMunger with Logging {

  // v: the sink vertex of this active hyperedge
  // parentReal: the realization at the parent, which we are proposing to add (traverse)
  override def traverseEdge(v: PackedVertex[Option[TaskTemplate]],
                            heOpt: Option[HyperEdge[Branch,SpecGroup]],
                            e: SpecGroup,
                            parentReal: Seq[Branch],
                            prevState: UnpackState): Option[UnpackState] = {

    debug("Vertex=%s; he=%s; Updating prevState: %s".format(v, heOpt, prevState))
    
    assert(prevState.edgeState.isEmpty, "There should be no previous edge state at this point")
    
    // left operand determines return type (an efficient immutable.HashMap)
    val result = new UnpackState(
        hyperedgeState = prevState.hyperedgeState,
        edgeState = UnpackState.emptyMap ++ parentReal.map { b: Branch => (b.branchPoint, b) } )
    debug("Initialized edgeState at " + v + ": " + result)
    Some(result)
  }
}

// merges the proposed realization from the edgeState into the hyperedgeState
object EdgeStateMerger extends HyperWorkflowStateMunger with Logging {

  // v: the sink vertex of this active hyperedge
  // parentReal: the realization at the parent, which we are proposing to add (traverse)
  override def traverseEdge(v: PackedVertex[Option[TaskTemplate]],
                            heOpt: Option[HyperEdge[Branch,SpecGroup]],
                            e: SpecGroup,
                            parentReal: Seq[Branch],
                            prevState: UnpackState): Option[UnpackState] = {

    // left operand determines return type (an efficient immutable.HashMap)
    val result = new UnpackState(
        hyperedgeState = prevState.hyperedgeState ++ prevState.edgeState,
        edgeState = UnpackState.emptyMap)
    debug("Vertex=%s; Merged edgeState into hyperedgeState %s ==> %s".format(v, prevState, result))
    Some(result)
  }
}