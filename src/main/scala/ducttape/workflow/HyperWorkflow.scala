package ducttape.workflow

import collection._
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.meta.MetaHyperDag
import ducttape.util.MultiSet
import ducttape.workflow.Types.UnpackState
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.Types.WorkflowEdge
import ducttape.workflow.Types.PackedWorkVert
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.PackageDef
import ducttape.syntax.AbstractSyntaxTree.SubmitterDef
import ducttape.syntax.AbstractSyntaxTree.VersionerDef
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.UnpackedVertex
import ducttape.hyperdag.meta.PhantomMetaHyperDag
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.hyperdag.walker.UnpackedPhantomMetaDagWalker
import ducttape.hyperdag.walker.PackedPhantomMetaDagWalker
import ducttape.hyperdag.walker.MetaVertexFilter
import ducttape.hyperdag.walker.RealizationMunger
import ducttape.workflow.SpecTypes.SpecPair

import grizzled.slf4j.Logging

trait PlanPolicy;
case class OneOff(graftRelaxations: Map[PackedWorkVert, Set[Branch]]) extends PlanPolicy;
case class VertexFilter(plannedVertices: Set[(String,Realization)]) extends PlanPolicy;
case class PatternFilter(
    planFilter: Map[BranchPoint, Set[String]],
    graftRelaxations: Map[PackedWorkVert, Set[Branch]]
  ) extends PlanPolicy;

// final type parameter TaskDef is for storing the source of input edges
// each element of plan is a set of branches that are mutually compatible
// - not specifying a branch point indicates that any value is acceptable
// TODO: Multimap (just use typedef?)
class HyperWorkflow(val dag: PhantomMetaHyperDag[TaskTemplate,BranchPoint,BranchInfo,Seq[SpecPair]],
                    val packageDefs: Map[String,PackageDef],
                    val plans: Seq[RealizationPlan],
                    val submitters: Seq[SubmitterDef], // TODO: Resolve earlier?
                    val versioners: Seq[VersionerDef],
                    val branchPointFactory: BranchPointFactory,
                    val branchFactory: BranchFactory)
    extends Logging {
  
  type UnpackedWalker = UnpackedPhantomMetaDagWalker[TaskTemplate,BranchPoint,BranchInfo,Seq[SpecPair],Branch,UnpackState]

  def packedWalker: PackedPhantomMetaDagWalker[TaskTemplate] = dag.packedWalker
  
  type HyperWorkflowMunger = RealizationMunger[Option[TaskTemplate], BranchInfo, Seq[SpecPair], Branch, UnpackState]
  
  trait HyperWorkflowStateMunger extends HyperWorkflowMunger {
    
    // heBranch might be None if this vertex has no incoming hyperedge
    override def initHyperedge(heBranch: Option[Branch]): UnpackState = heBranch match {
      case None => new UnpackState
      case Some(branch: Branch) => new UnpackState + ((branch.branchPoint, branch))
    }
    
    override def toRealization(state: UnpackState): Seq[Branch] = state.values.toSeq
  }
  
  /** when used with an unpacker, causes hyperedge grafts to be recognized
   *  and handled properly
   *  
   *  The explainCallback can be used to provide feedback to the user on
   *  why certain realizations were not produced (e.g. due to grafting). */
  class BranchGraftMunger(explainCallback: (=>String, =>String, Boolean) => Unit)
      extends HyperWorkflowStateMunger with Logging {
    
    // GAH! This returns a MultiSet[Branch] instead of an UnpackState... yet we want to affect the added parentRealization
    // How do we still keep MultiSets efficient while passing along this state and keeping it consistent?
    // Can we make the multiset more helpful (e.g. index it by BranchPoint?)
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
                heOpt.map { he => dag.delegate.delegate.sink(he).toString }.getOrElse("unknown"),
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

  // TODO: Currently only used by initial pass to find goals
  // TODO: Document different use cases of planFilter vs plannedVertices
  // NOTE: explainCallback can be used to provide the user with
  //       useful information about why certain realizations are not produced
  def NO_EXPLAIN(vertexName: => String, msg: => String, accepted: Boolean) {}
  def unpackedWalker(policy: PlanPolicy,
                     explainCallback: ( =>String, =>String, Boolean) => Unit = NO_EXPLAIN)
                     : UnpackedWalker = {
    
    // TODO: Should we allow access to "real" in this function -- that seems inefficient
    object GlobalBranchPointConstraint extends HyperWorkflowStateMunger {

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
    
    object InPlanConstraint extends HyperWorkflowStateMunger {

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

    // this is only used if we've previously made a pass to determine which vertices we'll be running
    val vertexFilter = new MetaVertexFilter[Option[TaskTemplate],BranchInfo,Seq[SpecPair],Branch] {
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
    }
    
    val ordering: Ordering[Branch] = Ordering.fromLessThan {
      (a: Branch, b: Branch) => a.branchPoint.name < b.branchPoint.name
    }
    
    // TODO: XXX: HACK: This shouldn't be called for nulls generated by epsilons
    def toD(branchInfo: BranchInfo): Branch = if (branchInfo != null) branchInfo.branch else Task.NO_BRANCH
    
    def observe(v: UnpackedVertex[Option[TaskTemplate], BranchInfo, Seq[SpecPair], Branch])
      = explainCallback(v.packed.toString, v.realization.mkString("-"), true)

    val munger = GlobalBranchPointConstraint.andThen(InPlanConstraint).andThen(new BranchGraftMunger(explainCallback))    
    dag.unpackedWalker[Branch,UnpackState](munger, vertexFilter, toD, observe)(ordering)
  }
}
