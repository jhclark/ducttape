package ducttape

import collection._
import ducttape.hyperdag._
//import ducttape.io._
import ducttape.workflow._

object Types {
  class HashMultiMap[A,B] extends mutable.HashMap[A,mutable.Set[B]] with mutable.MultiMap[A,B];

  class HyperWorkflow(val dag: MetaHyperDag[Task,BranchPoint,Branch,Null]);
  type UnpackedWorkVert = UnpackedVertex[Task,Branch,Null]
//
//  type LiteralSpec = SpecT[Literal]
//  type Spec = SpecT[RValue]
}
