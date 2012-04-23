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
import ducttape.hyperdag.walker._
import ducttape.hyperdag.HyperEdge

  // final type parameter TaskDef is for storing the source of input edges
  // each element of plan is a set of branches that are mutually compatible
  // - not specifying a branch point indicates that any value is acceptable
  // TODO: Multimap (just use typedef?)
  class HyperWorkflow(val dag: MetaHyperDag[TaskTemplate,BranchPoint,BranchInfo,Seq[Spec]],
                      val packageDefs: Map[String,PackageDef],
                      val plans: Seq[RealizationPlan],
                      val submitters: Seq[SubmitterDef], // TODO: Resolve earlier?
                      val versioners: Seq[VersionerDef],
                      val branchPointFactory: BranchPointFactory,
                      val branchFactory: BranchFactory) {

  def packedWalker = dag.packedWalker
    
    
  /** when used with an unpacker, causes anti-hyperedges to be recognized
   *  and handled properly (i.e. required if you want to use AntiHyperEdges) */
  object BranchGraftComboTransformer extends ComboTransformer[BranchInfo,Seq[Spec],Branch] {
    override def apply(heOpt: Option[WorkflowEdge], combo: MultiSet[Branch]) = heOpt match {
      case Some(he) => {
        if (he.h.grafts.size > 0) {
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
  // TODO: Return type
  def unpackedWalker(planFilter: Map[BranchPoint, Set[Branch]] = Map.empty,
                     plannedVertices: Set[(String,Realization)] = Set.empty) = {
    
    // TODO: Should we allow access to "real" in this function -- that seems inefficient
    val globalBranchPointConstraint = new ConstraintFilter[TaskTemplate,Branch,UnpackState] {
      override val initState = new UnpackState
      
      override def apply(v: PackedVertex[TaskTemplate],
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
          if(planFilter.size == 0) {
            true // size zero plan has special meaning
          } else {
            val ok = myReal.forall{ realBranch: Branch => planFilter.get(realBranch.branchPoint) match {
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
            if(!ok) {
              // TODO: XXX: Hack move to MetaHyperDAG
              val taskT: TaskTemplate = if(v.value == null) {
                dag.children(v).head.value
              } else {
                v.value
              }
              //Console.err.println("Plan excludes: "+myReal.mkString(" ")+ " at " + taskT)
            }
            ok
          }
        }
        if(parentReal.exists(violatesChosenBranch) || !inPlan(real.view ++ parentReal.view)) {
          None // we've already seen this branch point before -- and we just chose the wrong branch
        } else {
          //System.err.println("Extending seen: " + seen + " with " + parentReal + "Combo was: " + real)
          // left operand determines return type (an efficient immutable.HashMap)
          val result: UnpackState = seen ++ parentReal.map{b: Branch => (b.branchPoint, b)}
          Some(result)
        }
      }
    }

    val vertexFilter = new MetaVertexFilter[TaskTemplate,BranchInfo,Seq[Spec],Branch] {
      override def apply(v: UnpackedWorkVert): Boolean = {
        // TODO: Less extra work?
        val task = v.packed.value.realize(v)
        plannedVertices.contains( (task.name, task.realization) ) || plannedVertices.isEmpty
      } 
    }
    def toD(branchInfo: BranchInfo): Branch = branchInfo.branch
    dag.unpackedWalker[Branch,UnpackState](globalBranchPointConstraint, vertexFilter, BranchGraftComboTransformer, toD)
  }
}