package ducttape.hyperdag.meta

import collection._

import ducttape.hyperdag._

/** See [[ducttape.hyperdag.meta.MetaHyperDag]] for definitions of a MetaHyperDag, etc.
  * 
  * epsilonValue is just a dummy value that will never be given back to the user
  * (needed since we can't directly created objects with generic types)
  *
  * <img src="x.gif" /> */
class MetaHyperDagBuilder[V,M,H,E](epsilonV: V = null, epsilonH: H = null, epsilonE: E = null) {
  
  private val delegate = new HyperDagBuilder[V,H,E]
  private val metaEdgesByEpsilon = new mutable.HashMap[PackedVertex[_], MetaEdge[M,H,E]]
  private val metaEdgeSinks = new mutable.HashMap[PackedVertex[V], mutable.ArrayBuffer[PackedVertex[V]]]
  
  def addVertex(v: V, comment: Option[String] = None): PackedVertex[V] = delegate.addVertex(v, comment)

  /** hyperEdgeInfo is a sequence of component hyperedges, represented by pairs of (H, he_info)
   *    where he_info is a sequence of the component edges of each hyperedge
   *    (see [[ducttape.hyperdag.HyperDagBuilder.addEdge]] for more) */
  def addMetaEdge(m: M,
                  hyperEdgeInfo: Seq[(H, Seq[(PackedVertex[V],E)])],
                  sink: PackedVertex[V],
                  comment: Option[String]): MetaEdge[M,H,E] = {

    // TODO: Don't always alternate between normal and epsilon vertices to save some memory

    val meEpsilonV = delegate.addVertex(epsilonV, comment=comment)
    val hyperedges: Seq[HyperEdge[H,E]] = hyperEdgeInfo.map { heInfo =>
      val (h, edgeInfo) = heInfo
      edgeInfo.foreach { case (srcV, e) => assert(srcV != sink, "This meta-edge would create a cycle") }
      delegate.addHyperEdge(h, edgeInfo, meEpsilonV)
    }

    // associate this epsilon vertex with its metaedge
    val me = new MetaEdge[M,H,E](meEpsilonV, m, hyperedges)
    metaEdgesByEpsilon += meEpsilonV -> me

    // delay adding the meta-edges to the delegate builder
    // until we know we've accumulated all of them
    metaEdgeSinks.getOrElseUpdate(sink, {new mutable.ArrayBuffer[PackedVertex[V]]}) += meEpsilonV
    
    me
  }

  // create a usable immutable representation of this MetaHyperDag
  def build() = {
    // add single hyperedge that represents all incoming meta-edges
    // for all non-epsilon vertices
    val epsilonEdges: Set[HyperEdge[H,E]] = metaEdgeSinks.map { case (sink, epsilonVertices) =>
      val epsilonParents = epsilonVertices.map { v => (v, epsilonE) }
      delegate.addHyperEdge(epsilonH, epsilonParents, sink)
    }.toSet

    new MetaHyperDag[V,M,H,E](delegate.build(), metaEdgesByEpsilon, epsilonEdges)
  }
}
