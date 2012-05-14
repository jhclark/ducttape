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
    
  /** when used with an unpacker, causes anti-hyperedges to be recognized
   *  and handled properly (i.e. required if you want to use AntiHyperEdges) */
  object BranchGraftComboTransformer extends ComboTransformer[BranchInfo,Seq[SpecPair],Branch] with Logging {
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
            // this anti-hyperedge cannot apply
            //
            // TODO: Note when corresponding edge not found
            // to help user understand why no path is available
            None
          }
        }
      }
      case _ => Some(combo)
    }
  }

  // TODO: Currently only used by initial pass to find goals
  // TODO: Document different use cases of planFilter vs plannedVertices
  def unpackedWalker(planFilter: Map[BranchPoint, Set[Branch]] = Map.empty,
                     plannedVertices: Set[(String,Realization)] = Set.empty)
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
              case Some(planBranchesX: Set[_] /*Set[Branch]*/) => {
                val planBranches: Set[Branch] = planBranchesX
                planBranches.contains(realBranch)
              }
              // otherwise it implies the baseline branch
              case None => realBranch.name == Task.NO_BRANCH.name // compare *name*, not actual Baseline:baseline
            }}
            // TODO: Store such messages somewhere to optionally give a verbose
            // description of why some tasks don't run?
            if (!ok) {
              // TODO: XXX: Hack move to MetaHyperDAG
              val taskT: TaskTemplate = if (v.value == null) {
                dag.children(v).head.value.get
              } else {
                v.value.get
              }
              debug("Plan excludes: " + myReal.mkString(" ") + " at " + taskT)
            }
            ok
          }
        }
        
        if (parentReal.exists(violatesChosenBranch) || !inPlan(real.view ++ parentReal.view)) {
          None // we've already seen this branch point before -- and we just chose the wrong branch
        } else {
          debug("Extending seen: " + seen + " with " + parentReal + "Combo was: " + real)
          // left operand determines return type (an efficient immutable.HashMap)
          val result: UnpackState = seen ++ parentReal.map{b: Branch => (b.branchPoint, b)}
          Some(result)
        }
      }
    }

    val vertexFilter = new MetaVertexFilter[Option[TaskTemplate],BranchInfo,Seq[SpecPair],Branch] {
      override def apply(v: UnpackedMetaVertex[Option[TaskTemplate],BranchInfo,Seq[SpecPair],Branch]): Boolean = {
        // TODO: Less extraneous Realization creation?
        plannedVertices.contains( (v.packed.value.get.name, new Realization(v.realization)) ) || plannedVertices.isEmpty
      } 
    }
    
    // TODO: XXX: HACK: This shouldn't be called for nulls generated by epsilons
    def toD(branchInfo: BranchInfo): Branch = if (branchInfo != null) branchInfo.branch else Task.NO_BRANCH
    
    dag.unpackedWalker[Branch,UnpackState](
      constraintFilter=globalBranchPointConstraint,
      vertexFilter=vertexFilter,
      comboTransformer=BranchGraftComboTransformer,
      toD=toD)
  }
}