package ducttape.hyperdag

// 1) hierarchical agenda
// 2) level 1: active (input/init hypervertices + hypervertices that have at least 1 active unpacked vertex)
//             completed (hypervertices that have all of their unpacked vertices completed)
// 3) level 2: each hypervertex has a list of active arcs, which accumulate antecedent input files
//             and get moved to completed when all input files are satisfied for each realization
//             these second-level charts can also "handle" pairs of (realization, input files)
//             by doing nothing, indicating that it might not be in the selection, etc.

import collection._
import java.util.concurrent._

import ducttape.Types._

class PackedVertex[V](val value: V,
                      val antecedents: immutable.List[PackedVertex[V]]) {
  override def hashCode = value.hashCode
  override def equals(that: Any): Boolean = that match {
    case other: PackedVertex[V] => (other.value == this.value)
    case _ => false
  }
  override def toString = value.toString
}

// immutable
class PackedDag[V,U](val roots: immutable.List[PackedVertex[V]],
                     val vertices: immutable.List[PackedVertex[V]]) {
  val size: Int = vertices.size
  def walker(): PackedDagWalker[V] = new PackedDagWalker[V](this)
}

class PackedDagBuilder[V] {

  private val roots = new mutable.ListBuffer[PackedVertex[V]]
  private val vertices = new mutable.HashSet[PackedVertex[V]]

  def add(v: V, parents: PackedVertex[V]*): PackedVertex[V] = {
    require(parents.forall(p => vertices(p)), "Add parents first")
    val pv = new PackedVertex[V](v, parents.toList)
    require(!vertices(pv), "Packed vertices must be uniquely hashable")
    vertices += pv
    if(parents.size == 0) roots += pv
    pv
  }

  def build() = {
    new PackedDag[V,V](roots.toList, vertices.toList)
  }
}

/**
 * must be threadsafe
 */
trait Walker[A] extends Iterable[A] {

  private val self = this

  /**
   * returns None when there are no more elements
   */
  def take(): Option[A]

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
        nextItem = self.take
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

  private class ActiveVertex[P](val v: PackedVertex[P]) {
    assert(v != null)
    val filled = new Array[ActiveVertex[P]](v.antecedents.size)
  }

  // taken and agenda must always be jointly locked when updating since if both are zero,
  // it means we're done
  private val active = new mutable.HashMap[PackedVertex[P],ActiveVertex[P]] with mutable.SynchronizedMap[PackedVertex[P],ActiveVertex[P]]
  private val agenda = new ArrayBlockingQueue[ActiveVertex[P]](dag.size)
  private val taken = new mutable.HashSet[ActiveVertex[P]] with mutable.SynchronizedSet[ActiveVertex[P]]
  private val completed = new mutable.HashSet[ActiveVertex[P]] with mutable.SynchronizedSet[ActiveVertex[P]]

  // first, visit the roots
  for(root <- dag.roots.iterator) {
    val actRoot = new ActiveVertex[P](root)
    agenda.offer(actRoot)
    active += root -> actRoot
  }

  // forward pointers through trie (instead of just backpointers)
  // (mutable, but never changed after creation)
  val forward = new HashMultiMap[PackedVertex[P],PackedVertex[P]]
  for(v <- dag.vertices.iterator) {
    for(ant <- v.antecedents) {
      forward.addBinding(ant, v)
    }
  }

/*
  def getCompleted(): Iterable[PackedVertex[P]] = {
  }

  def getRunning(): Iterable[PackedVertex[P]] = {
  }

  def getReady(): Iterable[PackedVertex[P]] = {
  }

  def getBlocked(): Iterable[PackedVertex[P]] = {
    
  }
*/
  
  override def take(): Option[PackedVertex[P]] = {
    if(agenda.size == 0 && taken.size == 0) {
      return None
    } else {
      agenda.synchronized {
        val key: ActiveVertex[P] = agenda.take
        taken += key
        Some(key.v)
      }
    }
  }

  override def complete(item: PackedVertex[P]) = {
    require(active.contains(item), "Cannot find active vertex for %s in %s".format(item, active))
    val key: ActiveVertex[P] = active(item)

    // we always lock agenda & completed jointly
    agenda.synchronized {
      taken -= key
    }

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
        agenda.synchronized {
          agenda.offer(activeCon)
        }
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

/*
class UnpackedDagWalker[P] extends Walker[UnpackedVertex[P]] {
  // TODO: Test packed walker first (hashCode & equals might be an issue)
  // TODO: Then add RealizationDag as part of the state object?
  //       or is this already part of each vertex after building?
}
*/
