package ducttape

import collection._
import ducttape.hyperdag._
//import ducttape.io._
import ducttape.workflow._
import ducttape.util.MultiSet

object Types {
  class HashMultiMap[A,B] extends mutable.HashMap[A,mutable.Set[B]] with mutable.MultiMap[A,B];

  // when walking an unpacked DAG, only allow the same branch to appear in a realization once
  // By using Scala's fast immutable sets, we save big on space complexity while keeping reasonable time
  // See Phil Bagwell's work at EPFL on data sharing in immutable/persistent data structures
  type UnpackState = immutable.HashMap[BranchPoint, Branch]

  // TODO: Move this  
  class HyperWorkflow(val dag: MetaHyperDag[TaskTemplate,BranchPoint,Branch,Null]) {

    // TODO: Should we allow access to "real" in this function -- that seems inefficient
    def unpackFilter(seen: UnpackState, real: MultiSet[Branch], parentReal: Seq[Branch]): Option[UnpackState] = {
      assert(seen != null)
      assert(parentReal != null)
      assert(!parentReal.exists(_ == null))
      def violatesChosenBranch(newBranch: Branch) = seen.get(newBranch.branchPoint) match {
        case None => false // no branch chosen yet
        case Some(prevChosenBranch) => newBranch != prevChosenBranch
      }
      if(parentReal.exists(violatesChosenBranch)) {
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
  type UnpackedWorkVert = UnpackedVertex[TaskTemplate,Branch,Null]
//
//  type LiteralSpec = SpecT[Literal]
//  type Spec = SpecT[RValue]
}
