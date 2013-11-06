package ducttape.hyperdag

import collection._
import grizzled.slf4j.Logging

/**
 * see [[ducttape.hyperdag.HyperDag]] for the definition of a HyperDag.
 */
class HyperDagBuilder[V,H,E] extends Logging {

  private[hyperdag] val vertices = new mutable.HashSet[PackedVertex[V]]
  private val inEdges = new mutable.HashMap[PackedVertex[V],mutable.ListBuffer[HyperEdge[H,E]]]
  private val outEdges = new mutable.HashMap[PackedVertex[V],mutable.ListBuffer[HyperEdge[H,E]]]
  private val edges = new mutable.HashMap[HyperEdge[H,E], (Seq[PackedVertex[V]],PackedVertex[V])]
  private var vertexId = 0
  private var edgeId = 0

  // the comment is a GraphViz comment
  def addVertex(v: V, comment: Option[String] = None): PackedVertex[V] = {
    val pv = new PackedVertex[V](vertexId, v, comment)
    vertexId += 1
    debug("Add vertex: " + pv)
    vertices += pv
    pv
  }

  def addHyperEdge(h: H, sourcePairs: Seq[(PackedVertex[V],E)], sink: PackedVertex[V]): HyperEdge[H,E] = {
    val sources = sourcePairs.map { pair => pair._1 }
    val edgeLabels = sourcePairs.map { pair => pair._2 }

    require(sources.forall(v => vertices(v)), "Add sources first")
    require(vertices(sink), "Add sink first")
    require(sources.size > 0, "No sources specified")

    val he = new HyperEdge[H,E](edgeId, h, edgeLabels)
    edgeId += 1

    for (src <- sources) {
      assert(src != sink)
      outEdges.getOrElseUpdate(src, new mutable.ListBuffer) += he
    }
    inEdges.getOrElseUpdate(sink, new mutable.ListBuffer) += he
    debug("addHyperEdge: %s ===> %s".format(sources, sink))
    edges += he -> (sources, sink)
    he
  }

  // create an usable immutable version of this HyperDag
  def build() = {
    def inDegree(v: PackedVertex[V]): Int = inEdges.get(v) match {
      case None => 0
      case Some(hyperedges) => hyperedges.size
    }
    debug(vertices)
    debug(vertices.toSeq.map{ v => (v, inDegree(v)) })
    debug(inEdges)
    val roots = for (v <- vertices if inDegree(v) == 0) yield v
    assert(roots.size > 0 || vertices.size == 0, "No roots found for non-empty HyperDAG")
    new HyperDag[V,H,E](roots.toSeq, vertices.toSeq, inEdges.toMap, outEdges.toMap, edges.toMap)
  }
}
