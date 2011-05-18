#!/bin/sh
exec scala $0 $@
!#

import collection.mutable._

class BunchDag {
  def combinationIterator(): Seq[List[BunchVertex]] = {
    val visited = 
  }
}

class BunchVertex {
  
}

class HyperDag[V,E] (xxx: V) {
  val hypervertices = new ListBuffer[HyperVertex[V]]

  def add(v: V) = {
    
  }

  def hyperIterator(): Seq[HyperVertex[V]] = {
    for(hv <- hypervertices) yield {
      new HyperVertex[V];
    }
  }

  def iterator(): Seq[V] = {
    for(hv <- hypervertices) yield {
      xxx
    }
  }
}

class HyperVertex[V] {
  
}

class HyperEdge[E] {
      
}
