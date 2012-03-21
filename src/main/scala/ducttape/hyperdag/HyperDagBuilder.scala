package ducttape.hyperdag

import collection._

class HyperDagBuilder[V,H,E] {

  private val vertices = new mutable.HashSet[PackedVertex[V]]
  private val inEdges = new mutable.HashMap[PackedVertex[V],mutable.ListBuffer[HyperEdge[H,E]]]
  private val outEdges = new mutable.HashMap[PackedVertex[V],mutable.ListBuffer[HyperEdge[H,E]]]
  private val edges = new mutable.HashMap[HyperEdge[H,E], (Seq[PackedVertex[V]],PackedVertex[V])]
  private var vertexId = 0
  private var edgeId = 0

  // before adding hyperparents we must already know the realizations?
  // this seems to defeat the purpose of the builder...
  def addVertex(v: V): PackedVertex[V] = {
    val pv = new PackedVertex[V](vertexId, v)
    vertices += pv
    vertexId += 1
    pv
  }

  def addHyperEdge(h: H, sourcePairs: Seq[(PackedVertex[V],E)], sink: PackedVertex[V]): HyperEdge[H,E] = {
    val sources = for(pair <- sourcePairs) yield pair._1
    val edgeLabels = for(pair <- sourcePairs) yield pair._2

    require(sources.forall(v => vertices(v)), "Add sources first")
    require(vertices(sink), "Add sink first")

    val he = new HyperEdge[H,E](edgeId, h, edgeLabels)
    edgeId += 1

    for(src <- sources) {
      assert(src != sink)
      outEdges.getOrElseUpdate(src, new mutable.ListBuffer) += he
    }
    inEdges.getOrElseUpdate(sink, new mutable.ListBuffer) += he
    edges += he -> (sources, sink)
    he
  }

  def build() = {
    val roots = for(v <- vertices if !inEdges.contains(v)) yield v
    assert(roots.size > 0 || vertices.size == 0, "No roots found for non-empty HyperDAG")
    new HyperDag[V,H,E](roots.toList, vertices.toList, inEdges.toMap, outEdges.toMap, edges.toMap)
  }
}