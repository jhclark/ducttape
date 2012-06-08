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
  type UnpackState = immutable.HashMap[BranchPoint, Branch]
  
  type PackedWorkVert = PackedVertex[Option[TaskTemplate]]
  type UnpackedWorkVert = UnpackedChainedMetaVertex[TaskTemplate,BranchInfo,Seq[SpecPair],Branch]
  type WorkflowEdge = HyperEdge[BranchInfo,Seq[SpecPair]]
  type WorkflowMetaEdge = MetaEdge[BranchPoint, BranchInfo, Seq[SpecPair]]
}
