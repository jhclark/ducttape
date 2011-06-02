package ducttape

import collection._

//import ducttape.hyperdag._

object Types {
//  type RInstList = immutable.IndexedSeq[RInst]
  class HashMultiMap[A,B] extends mutable.HashMap[A,mutable.Set[B]] with mutable.MultiMap[A,B];
}
