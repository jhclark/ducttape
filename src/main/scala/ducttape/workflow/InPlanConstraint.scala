package ducttape.workflow

import HyperWorkflow._
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
import ducttape.hyperdag.walker.MetaVertexFilter
import ducttape.hyperdag.meta.UnpackedMetaVertex

class InPlanConstraint(
      policy: PlanPolicy,
      explainCallback: ExplainCallback)
    extends HyperWorkflowStateMunger
    with MetaVertexFilter[Option[TaskTemplate],BranchInfo,Seq[SpecPair],Branch]
    with Logging {
  
  // for MetaVertexFilter -- this is used iff we're using the VertexFilter policy
  override def apply(v: UnpackedMetaVertex[Option[TaskTemplate],BranchInfo,Seq[SpecPair],Branch]): Boolean = {
    policy match {
      case VertexFilter(plannedVertices) => {
        // TODO: Less extraneous Realization creation?
        val included = plannedVertices.contains( (v.packed.value.get.name, new Realization(v.realization)) )
        if (!included) {
          explainCallback(v.toString, "Plan excludes vertex", false)
        }
        included
      }
      // if we're not using the vertex filter, just pass through
      case _ => {
        trace("MetaVertexFilter vacuously accepts %s (not using a VertexFilter)".format(v))
        true
      }
    }
  } 

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
    
    trace("Applying inPlan filter at %s with hyperedge %s for realization: %s".format(v, heOpt, prevState.values.mkString("-")))
    
    def isGraftDependency(graftRelaxations: Map[PackedWorkVert, Set[Branch]],
        v: PackedWorkVert,
        branch: Branch): Boolean = {
      debug("Checking graft dependencies for " + v)
      graftRelaxations.get(v) match {
        case None => false
        case Some(grafts: Set[_] /* [Branch] */ ) => {
          debug("Found graft relaxation for %s due to branch graft".format(v, branch))
          grafts.contains(branch)
        }
      }
    }
    
    // TODO: Save a copy of which planFilters we haven't
    // violated yet in the state?
    // TODO: This could be much more efficient if we only
    // checked which 
    def inPlan(myReal: Traversable[Branch]): Boolean = {
      policy match {
        // just accept everything for now since vertex filter
        // will take care of keeping things manageable
        case VertexFilter(_) => true
        case OneOff(graftRelaxations: Map[PackedWorkVert, Set[Branch]]) => {
          trace("Checking if no more than one branch point selected a non-baseline branch (default one-off policy)")
          // note: graft dependencies don't count toward one non-baseline branch
          val nonBaselines = myReal.count {
            realBranch: Branch => !realBranch.baseline && !isGraftDependency(graftRelaxations, v, realBranch)
          }
          nonBaselines <= 1
        }
        case PatternFilter(planFilter: Map[BranchPoint, Set[String]],
                           graftRelaxations: Map[PackedWorkVert, Set[Branch]]) => {
          val ok = myReal.forall { realBranch: Branch =>
            planFilter.get(realBranch.branchPoint) match {
              // planFilter must explicitly mention a branch point
              case Some(planBranchesX: Set[_]) => {
                debug("Checking for explicit plan match")
                val planBranches: Set[String] = planBranchesX
                
                // TODO: Can move this dualistic baseline/name behavior somewhere more central? (and glob behavior too?)
                planBranches.contains("*") ||
                  planBranches.contains(realBranch.name) ||
                  (realBranch.baseline && planBranches.contains("baseline")) ||
                  isGraftDependency(graftRelaxations, v, realBranch)
              }
              // otherwise it implies either the baseline branch or a graft relaxation
              case None => realBranch.baseline || isGraftDependency(graftRelaxations, v, realBranch)
            }
          }
          if (!ok) {
            explainCallback(v.comment.getOrElse(v.value.getOrElse("Unknown").toString),
              "Plan excludes realization: %s".format(myReal.mkString("-")),
              false)
          }
          ok
        }
      }
    }
    
    // Use views so that we don't have to copy as much
    val myReal: Traversable[Branch] = prevState.values.view ++ parentReal.view
    if (!inPlan(myReal)) {
      None // it's not in the plan
    } else {
      // left operand determines return type (an efficient immutable.HashMap)
      val result: UnpackState = prevState ++ parentReal.map { b: Branch => (b.branchPoint, b) }
      trace("Extending seen at " + v + ": " + prevState.values + " with " + parentReal + " ==> " + result.values)
      Some(result)
    }
  }
}