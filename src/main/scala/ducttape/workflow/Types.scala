package ducttape.workflow

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
  type PackedWorkVert = PackedVertex[TaskTemplate]
  type UnpackedWorkVert = UnpackedMetaVertex[TaskTemplate,Branch,Seq[Spec]]
}
