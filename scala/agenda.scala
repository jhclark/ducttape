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

object GraphViz {
  def escape(str: String) = str.replace(' ', '_')
}

class PackedEdge[E](private val id: Int,
                    val value: E) {

  override def hashCode = id
  override def equals(that: Any): Boolean = that match {
    case other: PackedEdge[_] => (other.id == this.id)
    case _ => false
  }
  override def toString = value.toString
}

class PackedVertex[V](private val id: Int,
                      val value: V) {

  override def hashCode = id
  override def equals(that: Any): Boolean = that match {
    case other: PackedVertex[_] => (other.id == this.id)
    case _ => false
  }
  override def toString = value.toString
}

// immutable
class PackedDag[V,E](val roots: List[PackedVertex[V]],
                     val vertices: List[PackedVertex[V]],
                     private val inEdgesMap: Map[PackedVertex[V], Seq[PackedEdge[E]]],
                     private val outEdgesMap: Map[PackedVertex[V], Seq[PackedEdge[E]]],
                     private val edges:  Map[PackedEdge[E],(PackedVertex[V],PackedVertex[V])]) {
 
  val size: Int = vertices.size

  def walker(): PackedDagWalker[V]
    = new PackedDagWalker[V](this)
  def inEdges(v: PackedVertex[V]): Seq[PackedEdge[E]]
    = inEdgesMap.getOrElse(v, Seq.empty)
  def outEdges(v: PackedVertex[V]): Seq[PackedEdge[E]]
    = outEdgesMap.getOrElse(v, Seq.empty)
  def parents(v: PackedVertex[V]): Seq[PackedVertex[V]]
    = for(e <- inEdges(v)) yield edges(e)._1 // (source, sink)
  def children(v: PackedVertex[V]): Seq[PackedVertex[V]]
    = for(e <- outEdges(v)) yield edges(e)._2 // (source, sink)
  def source(e: PackedEdge[E]): PackedVertex[V] = edges(e)._1
  def sink(e: PackedEdge[E]): PackedVertex[V] = edges(e)._2

  def toGraphViz(): String = {
    val str = new StringBuilder(1000)
    str ++= "digraph G {\n"
    for(v <- vertices) {
      for(ant <- parents(v)) {
        str ++= GraphViz.escape(ant.toString) + " -> " + GraphViz.escape(v.toString) + "\n"
      }
    }
    str ++= "}\n"
    str.toString
  }
}

class PackedDagBuilder[V,E] {

  private val vertices = new mutable.HashSet[PackedVertex[V]]
  private val inEdges = new mutable.HashMap[PackedVertex[V],mutable.ListBuffer[PackedEdge[E]]]
  private val outEdges = new mutable.HashMap[PackedVertex[V],mutable.ListBuffer[PackedEdge[E]]]
  private val edges = new mutable.HashMap[PackedEdge[E],(PackedVertex[V],PackedVertex[V])]
  private var vertexId = 0
  private var edgeId = 0

  // before adding hyperparents we must already know the realizations?
  // this seems to defeat the purpose of the builder...
  def add(v: V): PackedVertex[V] = {
    val pv = new PackedVertex[V](vertexId, v)
    vertices += pv
    vertexId += 1
    pv
  }

  def add(e: E, source: PackedVertex[V], sink: PackedVertex[V]) : PackedEdge[E] = {
    require(vertices(source), "Add source first")
    require(vertices(sink), "Add sink first")
    val pe = new PackedEdge[E](edgeId, e)
    edgeId += 1
    outEdges.getOrElseUpdate(source, new mutable.ListBuffer) += pe
    inEdges.getOrElseUpdate(sink, new mutable.ListBuffer) += pe
    edges += pe -> (source, sink)
    pe
  }

  def build() = {
    val roots = for(v <- vertices if !inEdges.contains(v)) yield v
    new PackedDag[V,E](roots.toList, vertices.toList, inEdges.toMap, outEdges.toMap, edges.toMap)
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
      if(nextItem == None) nextItem = self.take
      nextItem != None
    }

    override def next(): A = {
      val hazNext = hasNext
      require(hazNext, "No more items. Call hasNext() first.")
      val result: A = nextItem.get
      nextItem = None
      self.complete(result)
      result
    }
  }
}

// agenda-based DAG iterator that allows for parallelization
class PackedDagWalker[V](dag: PackedDag[V,_]) extends Walker[PackedVertex[V]] {

  private class ActiveVertex(val v: PackedVertex[V]) {
    assert(v != null)
    val filled = new Array[ActiveVertex](dag.inEdges(v).size)
  }

  // taken and agenda must always be jointly locked when updating since if both are zero,
  // it means we're done
  private val active = new mutable.HashMap[PackedVertex[V],ActiveVertex]
                         with mutable.SynchronizedMap[PackedVertex[V],ActiveVertex]
  private val agenda = new ArrayBlockingQueue[ActiveVertex](dag.size)
  private val taken = new mutable.HashSet[ActiveVertex] with mutable.SynchronizedSet[ActiveVertex]
  private val completed = new mutable.HashSet[ActiveVertex] with mutable.SynchronizedSet[ActiveVertex]

  // first, visit the roots
  for(root <- dag.roots.iterator) {
    val actRoot = new ActiveVertex(root)
    agenda.offer(actRoot)
    active += root -> actRoot
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
  
  override def take(): Option[PackedVertex[V]] = {
    if(agenda.size == 0 && taken.size == 0) {
      return None
    } else {
      agenda.synchronized {
        val key: ActiveVertex = agenda.take
        taken += key
        Some(key.v)
      }
    }
  }

  override def complete(item: PackedVertex[V]) = {
    require(active.contains(item), "Cannot find active vertex for %s in %s".format(item, active))
    val key: ActiveVertex = active(item)

    // we always lock agenda & completed jointly
    agenda.synchronized {
      taken -= key
    }

    // first, match fronteir vertices
    for(consequent <- dag.children(key.v)) {
      val activeCon = active.getOrElseUpdate(consequent, new ActiveVertex(consequent))
      var allFilled = true
      for(i <- 0 until activeCon.filled.size) {
        val antecedents = dag.parents(consequent)
        if(key.v == antecedents(i)) {
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

class UnpackedVertex[V,E](val packed: PackedVertex[V],
                          val realization: List[RInst],
                          val parents: List[(PackedVertex[V],E,List[RInst])]);

class UnpackedDagWalker[V,E] extends Walker[UnpackedVertex[V]] {

  class ActiveVertex(val v: PackedVertex[V],
                     val realization: List[RInst]) {

    val filled = new Array[(PackedVertex[V],E,List[RInst])]
    def toUnpacked() = new UnpackedVertex[V,E](v, realization, filled.toList)
  }

  // TODO: PackingVertex spawns a ??? for each hyperedge completed
  // TODO: VanillaVertex spawns a ??? for each full incoming combo

  private val active = new mutable.HashMap[UnpackedVertex[V],ActiveVertex]
                         with mutable.SynchronizedMap[UnpackedVertex[V],ActiveVertex]
  // TODO: dag.size might not be big enough for this unpacked version...
  private val agenda = new ArrayBlockingQueue[ActiveVertex](dag.size)
  private val taken = new mutable.HashSet[ActiveVertex] with mutable.SynchronizedSet[ActiveVertex]
  private val completed = new mutable.HashSet[ActiveVertex] with mutable.SynchronizedSet[ActiveVertex]

  // first, visit the roots
  for(root <- dag.roots.iterator) {
    val actRoot = new ActiveVertex(root)
    agenda.offer(actRoot)
    active += root -> actRoot
  }

  override def take(): Option[UnpackedVertex[V,E]] = {
    if(agenda.size == 0 && taken.size == 0) {
      return None
    } else {
      agenda.synchronized {
        val key: ActiveVertex = agenda.take
        taken += key
        Some(key.toUnpacked)
      }
    }
  }

  override def complete(item: UnpackedVertex[V,E]) = {
    require(active.contains(item), "Cannot find active vertex for %s in %s".format(item, active))
    val key: ActiveVertex = active(item)

    // we always lock agenda & completed jointly
    agenda.synchronized {
      taken -= key
    }

    // 1) see if this completes any hyperedges

    // 2) see if this completed hyperedge completes any packing (meta?) edges

    // 3) for the newly completed packing edge, complete any new UnpackedDagVertices
    //    that come from the cross product of this new packing edge
    //    and all other orthogonal unpacked edges; these must be checked
    //    against the selection to make sure they're valid
    // Orthogonal: coming from other incoming *packed* edges
  }
}
