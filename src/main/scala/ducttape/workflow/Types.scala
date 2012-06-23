package ducttape.workflow

import collection._
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.util.MultiSet
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.meta.MetaEdge
import ducttape.hyperdag.meta.UnpackedChainedMetaVertex
import ducttape.workflow.SpecTypes.SpecPair

object Types {
  class HashMultiMap[A,B] extends mutable.HashMap[A,mutable.Set[B]] with mutable.MultiMap[A,B];

  // when walking an unpacked DAG, only allow the same branch to appear in a realization once
  // By using Scala's fast immutable sets, we save big on space complexity while keeping reasonable time
  // See Phil Bagwell's work at EPFL on data sharing in immutable/persistent data structures
  class UnpackState(
      // hyperedgeState: The branches we've already committed to adding
      val hyperedgeState: immutable.Map[BranchPoint, Branch],
      // edgeState: The branches from a particular edge that we're considering
      //   but might still get munged via grafting or rejected due to constraints
      val edgeState: immutable.Map[BranchPoint, Branch]) {
    override def toString() = "he=%s++e=%s".format(hyperedgeState.values.mkString("-"), edgeState.values.mkString("-"))
  }

  object UnpackState {
    val emptyMap = new immutable.HashMap[BranchPoint, Branch]
    val empty = new UnpackState(emptyMap, emptyMap)
  }
  
  type PackedWorkVert = PackedVertex[Option[TaskTemplate]]
  type UnpackedWorkVert = UnpackedChainedMetaVertex[TaskTemplate,Branch,SpecGroup,Branch]
  type WorkflowEdge = HyperEdge[Branch,SpecGroup]
  type WorkflowMetaEdge = MetaEdge[BranchPoint, Branch, SpecGroup]
}
