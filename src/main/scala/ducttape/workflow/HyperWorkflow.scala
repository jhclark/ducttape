package ducttape.workflow

import collection._
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.meta.MetaHyperDag
import ducttape.util.MultiSet
import ducttape.workflow.Types.UnpackState
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.Types.WorkflowEdge
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
        if (he.h == null || he.h.grafts.size == 0) { // TODO: Why is this null check necessary?
          // no grafting required. do nothing
          Some(combo)
        } else {
          if (he.h.grafts.forall(b => combo.contains(b))) {
            val copy = new MultiSet[Branch](combo)
            he.h.grafts.foreach(b => copy.removeAll(b))
            Some(copy)
          } else {
            // no corresponding edge was found in the derivation
            // this branch graft cannot apply
            graftFailureCallback("Realization filtered by branch graft: %s".format(combo.view.mkString("-")))
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
  def unpackedWalker(planFilter: Map[BranchPoint, Set[String]] = Map.empty,
                     plannedVertices: Set[(String,Realization)] = Set.empty,
                     realizationFailureCallback: => String => Unit = { x: String => ; })
                     : UnpackedWalker = {
    
    // TODO: Should we allow access to "real" in this function -- that seems inefficient
    val globalBranchPointConstraint = new ConstraintFilter[Option[TaskTemplate],Branch,UnpackState] {
      override val initState = new UnpackState
      
      override def apply(v: PackedVertex[Option[TaskTemplate]],
                         seen: UnpackState,
                         real: MultiSet[Branch],
                         parentReal: Seq[Branch]): Option[UnpackState] = {
        
        assert(seen != null)
        assert(parentReal != null)
        assert(!parentReal.exists(_ == null))
        
        // enforce that each branch point should atomically select one branch per hyperpath
        // through the (Meta)HyperDAG
        def violatesChosenBranch(newBranch: Branch) = seen.get(newBranch.branchPoint) match {
          case None => false // no branch chosen yet
          case Some(prevChosenBranch) => newBranch != prevChosenBranch
        }
        
        // TODO: Save a copy of which planFilters we haven't
        // violated yet in the state?
        // TODO: This could be much more efficient if we only
        // checked which 
        def inPlan(myReal: Traversable[Branch]): Boolean = {
          if (planFilter.size == 0) {
            true // size zero plan has special meaning
          } else {
            val ok = myReal.forall { realBranch: Branch => planFilter.get(realBranch.branchPoint) match {
              // planFilter must explicitly mention a branch point
              case Some(planBranchesX: Set[_]) => {
                val planBranches: Set[String] = planBranchesX
                // TODO: Can move this dualistic baseline/name behavior somewhere more central?
                planBranches.contains(realBranch.name) ||
                  (realBranch.baseline && planBranches.contains("baseline"))
              }
              // otherwise it implies the baseline branch
              case None => realBranch.baseline
            }}
            if (!ok) {
              realizationFailureCallback("Plan excludes realization: %s at %s".format(
                myReal.mkString(" "), v.comment.getOrElse(v.value.getOrElse("Unknown"))))
            }
            ok
          }
        }
        
        if (parentReal.exists(violatesChosenBranch) || !inPlan(real.view ++ parentReal.view)) {
          None // we've already seen this branch point before -- and we just chose the wrong branch
        } else {
          trace("Extending seen: " + seen + " with " + parentReal + "Combo was: " + real)
          // left operand determines return type (an efficient immutable.HashMap)
          val result: UnpackState = seen ++ parentReal.map{b: Branch => (b.branchPoint, b)}
          Some(result)
        }
      }
    }

    // this is only used if we've previously made a pass to determine which vertices we'll be running
    val vertexFilter = new MetaVertexFilter[Option[TaskTemplate],BranchInfo,Seq[SpecPair],Branch] {
      override def apply(v: UnpackedMetaVertex[Option[TaskTemplate],BranchInfo,Seq[SpecPair],Branch]): Boolean = {
        // TODO: Less extraneous Realization creation?
        realizationFailureCallback("Plan excludes vertex: %s".format(v))
        plannedVertices.contains( (v.packed.value.get.name, new Realization(v.realization)) ) || plannedVertices.isEmpty
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