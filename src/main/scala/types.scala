package ducttape

import collection._
import ducttape.hyperdag._
//import ducttape.io._
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.workflow._
import ducttape.util.MultiSet

object Types {
  class HashMultiMap[A,B] extends mutable.HashMap[A,mutable.Set[B]] with mutable.MultiMap[A,B];

  // when walking an unpacked DAG, only allow the same branch to appear in a realization once
  // By using Scala's fast immutable sets, we save big on space complexity while keeping reasonable time
  // See Phil Bagwell's work at EPFL on data sharing in immutable/persistent data structures
  type UnpackState = immutable.HashMap[BranchPoint, Branch]

  // TODO: Move this  
  // final type parameter TaskDef is for storing the source of input edges
  // each element of plan is a set of branches that are mutually compatible
  // - not specifying a branch point indicates that any value is acceptable
  // TODO: Multimap (just use typedef?)
  class HyperWorkflow(val dag: MetaHyperDag[TaskTemplate,BranchPoint,Branch,Seq[Spec]],
                      val planFilter: Seq[Map[BranchPoint,Set[Branch]]]) {

    // TODO: Should we allow access to "real" in this function -- that seems inefficient
    def unpackFilter(seen: UnpackState, real: MultiSet[Branch], parentReal: Seq[Branch]): Option[UnpackState] = {
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
      def violatesPlan(myReal: Traversable[Branch]): Boolean = {
        // there must be at least one element in the plan...
        planFilter.exists{ planElement: Map[BranchPoint, Set[Branch]] => {
          // ...that covers all of the branches in the proposed realization...
          myReal.forall{realBranch => planElement.get(realBranch.branchPoint) match {
            // ...by explicitly mentioning it...
            case Some(planBranches: Set[Branch]) => planBranches.contains(realBranch)
            // or implying the baseline branch
            case None => realBranch == Task.NO_BRANCH
          }}
        }}
      }
      if(parentReal.exists(violatesChosenBranch) || violatesPlan(real.view ++ parentReal.view)) {
        None // we've already seen this branch point before -- and we just chose the wrong branch
      } else {
        //System.err.println("Extending seen: " + seen + " with " + parentReal + "Combo was: " + real)
        Some(seen ++ parentReal.map(b => (b.branchPoint, b))) // left operand determines return type
      }
    }

    def packedWalker = dag.packedWalker
    def unpackedWalker = dag.unpackedWalker[UnpackState](new UnpackState, unpackFilter)
  }
  type PackedWorkVert = PackedVertex[TaskTemplate]
  type UnpackedWorkVert = UnpackedMetaVertex[TaskTemplate,Branch,Seq[Spec]]
//
//  type LiteralSpec = SpecT[Literal]
//  type Spec = SpecT[RValue]
}
