
// 1) hierarchical agenda
// 2) level 1: active (input/init hypervertices + hypervertices that have at least 1 active unpacked vertex)
//             completed (hypervertices that have all of their unpacked vertices completed)
// 3) level 2: each hypervertex has a list of active arcs, which accumulate antecedent input files
//             and get moved to completed when all input files are satisfied for each realization
//             these second-level charts can also "handle" pairs of (realization, input files)
//             by doing nothing, indicating that it might not be in the selection, etc.

import collection.mutable._

class HashMultiMap[A,B] extends HashMap[A,Set[B]] with MultiMap[A,B];

class PackedVertex[P] {
  val antecedents = new ListBuffer[PackedVertex[P]]
}

// immutable
class PackedDag[P,U] {
  val roots = new ListBuffer[PackedVertex[P]]
  val vertices = new ListBuffer[PackedVertex[P]]
  def size(): Int = {
    0
  }
}

// agenda-based DAG iterator
class PackedDagIterator[P](dag: PackedDag[P,_]) extends Iterator[PackedVertex[P]] {

  class ActiveVertex[P](val v: PackedVertex[P]) {
    val filled = new Array[ActiveVertex[P]](v.antecedents.size)
  }

  val active = new HashMap[PackedVertex[P],ActiveVertex[P]]
  val agenda = new Queue[ActiveVertex[P]]
  val completed = new HashSet[ActiveVertex[P]]

  // first, visit the roots
  for(root <- dag.roots.iterator) {
    agenda += new ActiveVertex[P](root)
  }

  // forward pointers through trie (instead of just backpointers)
  val forward = new HashMultiMap[PackedVertex[P],PackedVertex[P]]
  for(v <- dag.vertices.iterator) {
    for(ant <- v.antecedents) {
      forward.addBinding(ant, v)
    }
  }

  override def hasNext() = agenda.size > 0

  override def next(): PackedVertex[P] = {
    require(hasNext, "No more items. Call hasNext() first.")
    val key: ActiveVertex[P] = agenda.dequeue
    
    // first, match fronteir vertices
    for(consequent <- forward.getOrElse(key.v, Set.empty)) {
      val activeCon = active.getOrElseUpdate(consequent, new ActiveVertex[P](consequent))
      var allFilled = true
      for(i <- 0 until activeCon.filled.size) {
        if(key.v == consequent.antecedents(i)) {
          activeCon.filled(i) = key
        } else if(activeCon.filled(i) == null) {
          allFilled = false
        }
      }
      // this consequent has all its dependencies fulfilled
      if(allFilled) {
        agenda += activeCon
        // TODO: We could sort the agenda here to impose different objectives...
      }
    }

    // finally visit this vertex
    completed += key
    key.v
  }
}

class UnpackedDagIterator[U] {
  
}

class Active {
}
