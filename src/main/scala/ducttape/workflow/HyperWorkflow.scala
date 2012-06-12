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
import ducttape.hyperdag.meta.PhantomMetaHyperDag
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.hyperdag.walker.UnpackedPhantomMetaDagWalker
import ducttape.hyperdag.walker.PackedPhantomMetaDagWalker
import ducttape.hyperdag.walker.ComboTransformer
import ducttape.hyperdag.walker.ConstraintFilter
import ducttape.hyperdag.walker.MetaVertexFilter
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
    
  /** when used with an unpacker, causes hyperedge grafts to be recognized
   *  and handled properly
   *  
   *  The branchFailureCallback can be used to provide feedback to the user on
   *  why certain realizations were not produced. */
  class BranchGraftComboTransformer(graftFailureCallback: => String => Unit)
      extends ComboTransformer[BranchInfo,Seq[SpecPair],Branch]
      with Logging {
    
    override def apply(heOpt: Option[WorkflowEdge], combo: MultiSet[Branch]) = heOpt match {
      case Some(he) => {
        trace {
          val sink = dag.delegate.delegate.sink(he)
          "Considering if we need to apply a graft for he '%s' with sink '%s': ".format(he, sink, combo)
        }
        if (he.h == null || he.h.grafts.size == 0) { // TODO: Why is this null check necessary?
          // no grafting required. do nothing
          trace("No grafting required")
          Some(combo)
        } else {
          if (he.h.grafts.forall { branch => combo.contains(branch) } ) {
            val copy = new MultiSet[Branch](combo)
            he.h.grafts.foreach { branch => copy.removeAll(branch) }
            trace("Applied grafts: %s => %s".format(combo.keys, copy.keys))
            Some(copy)
          } else {
            trace("Filtered by branch graft")
            // no corresponding edge was found in the derivation
            // this branch graft cannot apply
            graftFailureCallback {
              val sinkName = heOpt.map { he => dag.sink(he).toString }.getOrElse("unknown")
              "Realization %s feeding %s filtered by branch graft: %s".format(
                combo.view.mkString("-"), sinkName, he.h.grafts.mkString(","))
            }
            None
          }
        }
      }
      case _ => Some(combo)
    }
  }

  // TODO: Currently only used by initial pass to find goals
  // TODO: Document different use cases of planFilter vs plannedVertices
  // NOTE: realizationFailureCallback can be used to provide the user with
  //       useful information about why certain realizations are not produced
  def unpackedWalker(policy: PlanPolicy,
                     realizationFailureCallback: => String => Unit = { x: String => ; })
                     : UnpackedWalker = {
    
    // TODO: globalBranchPointConstraint is also the inPlanConstraint
    // TODO: Should we allow access to "real" in this function -- that seems inefficient
    val globalBranchPointConstraint = new ConstraintFilter[Option[TaskTemplate],Branch,UnpackState] {
      override val initState = new UnpackState
      
      // real: the current realization of this vertex
      // seen: the non-local derivation state we pass around (more efficient to access than real)
      // parentReal: the realization at the parent, which we are proposing to add (traverse)
      override def apply(v: PackedVertex[Option[TaskTemplate]],
                         seen: UnpackState,
                         real: MultiSet[Branch],
                         parentReal: Seq[Branch]): Option[UnpackState] = {
        
        assert(seen != null)
        assert(parentReal != null)
        assert(!parentReal.exists(_ == null))
        
        trace("Applying constraint filter at %s for realization: %s".format(v, real.view.mkString("-")))
        
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
        
        def isGraftDependency(graftRelaxations: Map[PackedWorkVert, Set[Branch]],
            v: PackedWorkVert,
            branch: Branch): Boolean = {
          debug("Checking graft dependencies for " + v)
          graftRelaxations.get(v) match {
            case None => false
            case Some(grafts: Set[Branch]) => {
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
                realizationFailureCallback("Plan excludes realization: %s at %s".format(
                  myReal.mkString(" "), v.comment.getOrElse(v.value.getOrElse("Unknown"))))
              }
              ok
            }
          }
        }
        
        if (parentReal.exists { branch => violatesChosenBranch(seen, branch) }
            || !inPlan(real.view ++ parentReal.view)) {
          None // we've already seen this branch point before -- and we just chose the wrong branch
        } else {
          // left operand determines return type (an efficient immutable.HashMap)
          val result: UnpackState = seen ++ parentReal.map { b: Branch => (b.branchPoint, b) }
          trace("Extending seen: " + seen.values + " with " + parentReal + "; Combo was " + real.keys + " ==> " + result.values)
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
              realizationFailureCallback("Plan excludes vertex: %s".format(v))
            }
            included
          }
          // if we're not using the vertex filter, just pass through
          case _ => true
        }
      } 
    }
    
    val ordering: Ordering[Branch] = Ordering.fromLessThan {
      (a: Branch, b: Branch) => a.branchPoint.name < b.branchPoint.name
    }
    
    // TODO: XXX: HACK: This shouldn't be called for nulls generated by epsilons
    def toD(branchInfo: BranchInfo): Branch = if (branchInfo != null) branchInfo.branch else Task.NO_BRANCH
    
    dag.unpackedWalker[Branch,UnpackState](
      constraintFilter=globalBranchPointConstraint,
      vertexFilter=vertexFilter,
      comboTransformer=new BranchGraftComboTransformer(realizationFailureCallback),
      toD=toD)(ordering)
  }
}
