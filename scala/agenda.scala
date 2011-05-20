
// 1) hierarchical agenda
// 2) level 1: active (input/init hypervertices + hypervertices that have at least 1 active unpacked vertex)
//             completed (hypervertices that have all of their unpacked vertices completed)
// 3) level 2: each hypervertex has a list of active arcs, which accumulate antecedent input files
//             and get moved to completed when all input files are satisfied for each realization
//             these second-level charts can also "handle" pairs of (realization, input files)
//             by doing nothing, indicating that it might not be in the selection, etc.

import collection.mutable._
import java.util.concurrent._

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

/**
 * must be threadsafe
 */
trait Walker[A] extends Iterable[A] {

  private val self = this

  /**
   * returns null when there are no more elements
   */
  def take(): A

  /**
   * notify walker that caller is done with the item so that we know we
   * can traverse its dependends
   */
  def complete(item: A)

  /**
   * get a synchronous iterator (not appropriate for multi-threaded consumers)
   */
  def iterator() = new Iterator[A] {
    var nextItem: Option[A] = None

    override def hasNext(): Boolean = {
      if(nextItem == None) {
        // NOTE: This could block infinitely for buggy Walkers
        nextItem = Some(self.take)
      }
      nextItem != None
    }

    override def next(): A = {
      val hazNext = hasNext
      require(hazNext, "No more items. Call hasNext() first.")
      val result:A = nextItem.get
      nextItem = None
      self.complete(result)
      result
    }
  }
}

// agenda-based DAG iterator that allows for parallelization
class PackedDagWalker[P](dag: PackedDag[P,_]) extends Walker[PackedVertex[P]] {

  class ActiveVertex[P](val v: PackedVertex[P]) {
    val filled = new Array[ActiveVertex[P]](v.antecedents.size)
  }

  val active = new HashMap[PackedVertex[P],ActiveVertex[P]]
  val agenda = new ArrayBlockingQueue[ActiveVertex[P]](dag.size)
  val completed = new HashSet[ActiveVertex[P]]

  // first, visit the roots
  for(root <- dag.roots.iterator) {
    agenda.offer(new ActiveVertex[P](root))
  }

  // forward pointers through trie (instead of just backpointers)
  val forward = new HashMultiMap[PackedVertex[P],PackedVertex[P]]
  for(v <- dag.vertices.iterator) {
    for(ant <- v.antecedents) {
      forward.addBinding(ant, v)
    }
  }

  override def take(): PackedVertex[P] = {
    val key: ActiveVertex[P] = agenda.poll
    key.v
  }

  override def complete(item: PackedVertex[P]) = {
    val key: ActiveVertex[P] = active(item)

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
        agenda.offer(activeCon)
        // TODO: We could sort the agenda here to impose different objectives...
      }
    }

    // finally visit this vertex
    completed += key
  }
}

class UnpackedVertex[P](val packed: PackedVertex[P],
                        val realization: List[RInst],
                        val antecedents: List[UnpackedVertex[P]]);

class UnpackedDagWalker[P] extends Walker[UnpackedVertex[P]] {
  // TODO: Test packed walker first (hashCode & equals might be an issue)
  // TODO: Then add RealizationDag as part of the state object?
  //       or is this already part of each vertex after building?
}
