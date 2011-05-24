package ducttape.hyperdag

import collection._
import scala.collection.JavaConversions._
import java.util.concurrent._

import ducttape.Types._
import ducttape.viz._

class HyperEdge[H,E](private val id: Int, val h: H, val e: List[E]) {
  override def hashCode = id
  override def equals(that: Any) = that match { case other: HyperEdge[_,_] => (other.id == this.id) }
  override def toString = h.toString + " " + e.toString
}

class PackedVertex[V](private val id: Int, val value: V) {
  override def hashCode = id
  override def equals(that: Any) = that match { case other: PackedVertex[_] => (other.id == this.id) }
  override def toString = value.toString
}

// immutable
class PackedDag[V,H,E](val roots: List[PackedVertex[V]],
                       val vertices: List[PackedVertex[V]],
                       private val inEdgesMap: Map[PackedVertex[V], Seq[HyperEdge[H,E]]],
                       private val outEdgesMap: Map[PackedVertex[V], Seq[HyperEdge[H,E]]],
                       private val edges: Map[HyperEdge[H,E], (List[PackedVertex[V]],PackedVertex[V])]) {
                       
  val size: Int = vertices.size

  def walker(): PackedDagWalker[V]
    = new PackedDagWalker[V](this)
  def inEdges(v: PackedVertex[V]): Seq[HyperEdge[H,E]]
    = inEdgesMap.getOrElse(v, Seq.empty)
  def outEdges(v: PackedVertex[V]): Seq[HyperEdge[H,E]]
    = outEdgesMap.getOrElse(v, Seq.empty)
  def parents(v: PackedVertex[V]): Seq[PackedVertex[V]] = for(e <- inEdges(v); src <- sources(e)) yield src
  def children(v: PackedVertex[V]): Seq[PackedVertex[V]]
    = for(e <- outEdges(v)) yield sink(e)
  def sources(e: HyperEdge[H,E]): Seq[PackedVertex[V]] = edges(e)._1
  def sink(e: HyperEdge[H,E]): PackedVertex[V] = edges(e)._2

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

class PackedDagBuilder[V,H,E] {

  private val vertices = new mutable.HashSet[PackedVertex[V]]
  private val inEdges = new mutable.HashMap[PackedVertex[V],mutable.ListBuffer[HyperEdge[H,E]]]
  private val outEdges = new mutable.HashMap[PackedVertex[V],mutable.ListBuffer[HyperEdge[H,E]]]
  private val edges = new mutable.HashMap[HyperEdge[H,E], (List[PackedVertex[V]],PackedVertex[V])]
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

  def add(h: H, e: List[E], sources: List[PackedVertex[V]], sink: PackedVertex[V]): HyperEdge[H,E] = {
    require(sources.forall(v => vertices(v)), "Add sources first")
    require(vertices(sink), "Add sink first")
    val he = new HyperEdge[H,E](edgeId, h, e)
    edgeId += 1
    for(src <- sources) outEdges.getOrElseUpdate(src, new mutable.ListBuffer) += he
    inEdges.getOrElseUpdate(sink, new mutable.ListBuffer) += he
    edges += he -> (sources, sink)
    he
  }

  def build() = {
    val roots = for(v <- vertices if !inEdges.contains(v)) yield v
    new PackedDag[V,H,E](roots.toList, vertices.toList, inEdges.toMap, outEdges.toMap, edges.toMap)
  }
}

// agenda-based DAG iterator that allows for parallelization
class PackedDagWalker[V](dag: PackedDag[V,_,_]) extends Walker[PackedVertex[V]] {

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

  def getCompleted(): Traversable[PackedVertex[V]] = for(act <- completed) yield act.v
  def getRunning(): Traversable[PackedVertex[V]] = for(act <- taken) yield act.v
  def getReady(): Traversable[PackedVertex[V]] = for(act <- agenda) yield act.v
//  def getBlocked(): Traversable[PackedVertex[P]] = 
  
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

// this interface explicitly avoids giving unpacked vertices as
// parents so that we can eventually discard more of the explored space
class UnpackedVertex[V,H,E](val packed: PackedVertex[V],
                          val edge: HyperEdge[H,E],
                          val realization: List[H],
                          val parentRealizations: List[List[H]]);

class UnpackedDagWalker[V,H,E](val dag: PackedDag[V,H,E])
  extends Walker[UnpackedVertex[V,H,E]]{

  class ActiveVertex(val v: PackedVertex[V],
                     val realization: List[H]) {

//    val filled = new Array[(PackedVertex[V],E,List[RInst])]()
    def toUnpacked() = new UnpackedVertex[V,H,E](v, realization, filled.toList)
  }

  // TODO: PackingVertex spawns a ??? for each hyperedge completed
  // TODO: VanillaVertex spawns a ??? for each full incoming combo

  private val active = new mutable.HashMap[UnpackedVertex[V,H,E],ActiveVertex]
                         with mutable.SynchronizedMap[UnpackedVertex[V,H,E],ActiveVertex]
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

  override def take(): Option[UnpackedVertex[V,H,E]] = {
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

  override def complete(item: UnpackedVertex[V,H,E]) = {
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
