package ducttape.hyperdag

import collection._

import ducttape.viz._

class HyperEdge[H,E](private[hyperdag] val id: Int, val h: H, val e: List[E]) {
  override def hashCode = id
  override def equals(that: Any) = that match { case other: HyperEdge[_,_] => (other.id == this.id) }
  override def toString = h.toString + " " + e.toString
}

class PackedVertex[V](private[hyperdag] val id: Int, val value: V) {
  override def hashCode = id
  override def equals(that: Any) = that match { case other: PackedVertex[_] => (other.id == this.id) }
  override def toString = value.toString
}

// this interface explicitly avoids giving unpacked vertices as
// parents so that we can eventually discard more of the explored space
class UnpackedVertex[V,H,E](val packed: PackedVertex[V],
                            val edge: Option[HyperEdge[H,E]],
                            val realization: Seq[H],
                            val parentRealizations: Seq[Seq[H]]) {
  // TODO: More smearing of hash codes
  override def hashCode = packed.id ^ realization.hashCode
  override def equals(that: Any) = that match {
    case other: UnpackedVertex[_,_,_] => (other.packed.id == this.packed.id) && (other.realization == this.realization)
  }
  override def toString = packed.toString + " (realization=" + realization.toString +")"
}

// immutable
class PackedDag[V,H,E](val roots: List[PackedVertex[V]],
                       val vertices: List[PackedVertex[V]],
                       private val inEdgesMap: Map[PackedVertex[V], Seq[HyperEdge[H,E]]],
                       private val outEdgesMap: Map[PackedVertex[V], Seq[HyperEdge[H,E]]],
                       private val edges: Map[HyperEdge[H,E], (List[PackedVertex[V]],PackedVertex[V])]) {
                       
  val size: Int = vertices.size

  def packedWalker()
    = new PackedDagWalker[V](this)
  // TODO: Pass filters to dag walker
  def unpackedWalker()
    = new UnpackedDagWalker[V,H,E](this)
  def inEdges(v: PackedVertex[V]): Seq[HyperEdge[H,E]]
    = inEdgesMap.getOrElse(v, Seq.empty)
  def outEdges(v: PackedVertex[V]): Seq[HyperEdge[H,E]]
    = outEdgesMap.getOrElse(v, Seq.empty)
  def parents(v: PackedVertex[V]): Seq[PackedVertex[V]]
    = for(e <- inEdges(v); src <- sources(e)) yield src
  def children(v: PackedVertex[V]): Seq[PackedVertex[V]]
    = for(e <- outEdges(v)) yield sink(e)
  def sources(e: HyperEdge[H,E]): Seq[PackedVertex[V]]
    = edges(e)._1
  def sink(e: HyperEdge[H,E]): PackedVertex[V]
    = edges(e)._2

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

  def add(h: H, sourcePairs: List[(PackedVertex[V],E)], sink: PackedVertex[V]): HyperEdge[H,E] = {
    val sources = for(pair <- sourcePairs) yield pair._1
    val edgeLabels = for(pair <- sourcePairs) yield pair._2

    require(sources.forall(v => vertices(v)), "Add sources first")
    require(vertices(sink), "Add sink first")

    val he = new HyperEdge[H,E](edgeId, h, edgeLabels)
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
