package ducttape.workflow

import collection._

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
import ducttape.hyperdag.walker.MetaVertexFilter
import ducttape.hyperdag.meta.UnpackedMetaVertex

import grizzled.slf4j.Logging

// TODO: Move these to top-level classes
trait PlanPolicy;
case class OneOff(graftRelaxations: Map[PackedWorkVert, Set[Branch]]) extends PlanPolicy;
case class VertexFilter(plannedVertices: Set[RealTaskId]) extends PlanPolicy;
case class PatternFilter(
    planFilter: Map[BranchPoint, Set[String]],
    graftRelaxations: Map[PackedWorkVert, Set[Branch]]
  ) extends PlanPolicy;

class InPlanConstraint(policy: PlanPolicy, explainCallback: ExplainCallback)
    extends HyperWorkflowStateMunger
    with MetaVertexFilter[Option[TaskTemplate],Branch,SpecGroup,Branch]
    with Logging {
  
  // for MetaVertexFilter -- this is used iff we're using the VertexFilter policy
  override def apply(v: UnpackedMetaVertex[Option[TaskTemplate],Branch,SpecGroup,Branch]): Boolean = {
    val taskT: TaskTemplate = v.packed.value.get
    policy match {
      case VertexFilter(plannedVertices) => {
        // TODO: Less extraneous Realization creation?
        val included = plannedVertices.contains(new RealTaskId(taskT.name, v.realization))
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
                            heOpt: Option[HyperEdge[Branch,SpecGroup]],
                            e: SpecGroup,
                            parentReal: Seq[Branch],
                            prevState: UnpackState): Option[UnpackState] = {
    
    assert(prevState != null)
    assert(parentReal != null)
    assert(!parentReal.exists(_ == null))
    
    trace("Applying inPlan filter at %s with hyperedge %s for realization: %s".format(v, heOpt, prevState))
    
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
    // Set because these must be uniquified
    def inPlan(myReal: Set[Branch]): Boolean = {
      policy match {
        // just accept everything for now since vertex filter
        // will take care of keeping things manageable
        case VertexFilter(_) => true
        case OneOff(graftRelaxationsX) => {
          val graftRelaxations: Map[PackedWorkVert, Set[Branch]] = graftRelaxationsX
          // note: graft dependencies don't count toward one non-baseline branch
          val nonBaselines = myReal.count {
            realBranch: Branch => !realBranch.baseline && !isGraftDependency(graftRelaxations, v, realBranch)
          }
          if (isDebugEnabled) {
            debug("OneOff: Number of non-baseline branches: %d".format(nonBaselines))
            myReal.foreach { realBranch: Branch => debug("OneOff: Branch %s: baseline? %s graftDep? %s".format(
                realBranch, realBranch.baseline, isGraftDependency(graftRelaxations, v, realBranch))) }
          }
          nonBaselines <= 1
        }
        case PatternFilter(planFilterX, graftRelaxationsX) => {
          val planFilter: Map[BranchPoint, Set[String]] = planFilterX
          val graftRelaxations: Map[PackedWorkVert, Set[Branch]] = graftRelaxationsX
                             
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
          ok
        }
      }
    }
    
    // Use views so that we don't have to copy as much
    // note that edgeState has been merged into the hyperedgeState by this point
    val myReal: Set[Branch] = (prevState.hyperedgeState.values.view ++ parentReal.view).toSet
    if (!inPlan(myReal)) {
      explainCallback(v.comment.getOrElse(v.value.getOrElse("Unknown").toString),
        "Plan excludes realization: %s".format(myReal.mkString("-")),
        false)
      debug("Vertex=%s; Not in plan: %s".format(v, prevState))
      None // it's not in the plan
    } else {
      debug("Vertex=%s; In plan: %s".format(v, prevState))
      Some(prevState)
    }
  }
  
//  override def finishHyperedge(v: PackedWorkVert, heOpt: WorkflowEdge, state: UnpackState): Option[UnpackState] = {
//    
//    Some(state)
//  }
}
