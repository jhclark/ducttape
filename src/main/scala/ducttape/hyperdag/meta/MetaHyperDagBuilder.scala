package ducttape.hyperdag.meta

import collection._

import ducttape.hyperdag._

/** epsilonValue is just a dummy value that will never be given back to the user
  * (needed since we can't directly created objects with generic types)
  *
  * <img src="x.gif" /> */
class MetaHyperDagBuilder[V,M,H,E](private val epsilonV: V = null,
                                   private val epsilonH: H = null,
                                   private val epsilonE: E = null) {
  private val delegate = new HyperDagBuilder[V,H,E]
  private val metaEdgesByEpsilon = new mutable.HashMap[PackedVertex[V], MetaEdge[M,H,E]]
  private val metaEdgeSinks = new mutable.HashMap[PackedVertex[V], mutable.ArrayBuffer[PackedVertex[V]]]
  private val phantomVertices = new mutable.HashSet[PackedVertex[V]]
  
  def addVertex(v: V): PackedVertex[V] = delegate.addVertex(v)

  // phantom edge: an edge with no source vertices
  // phantom vertex: the source vertex of a phantom edge
  // used in ducttape for literal file paths, which may be linked with branches
  // but don't have any executable code associated with them
  // TODO: Should this be incorporated into HyperDAG, too?
  // this method allows sources to be optional, indicating that edges *may*
  // have phantom source vertices
  def addPhantomMetaEdge(m: M,
		                 hyperEdgeInfo: Seq[(H, Seq[(Option[PackedVertex[V]],E)])],
		                 sink: PackedVertex[V]): MetaEdge[M,H,E] = {
    val hyperEdgeInfoP = for( (h, edgeInfo) <- hyperEdgeInfo) yield {
      val edgeInfoP = for( (vOpt, e) <- edgeInfo) yield {
        vOpt match {
          case Some(v) => (v, e)
          case None => {
            val phantomV = delegate.addVertex(epsilonV)
            phantomVertices += phantomV
            (phantomV, e)
          }
        }
      }
      (h, edgeInfoP)
    }
    addMetaEdge(m, hyperEdgeInfoP, sink)
  }

  def addMetaEdge(m: M,
                  hyperEdgeInfo: Seq[(H, Seq[(PackedVertex[V],E)])],
                  sink: PackedVertex[V]): MetaEdge[M,H,E] = {

    // TODO: Don't always alternate between normal and epsilon vertices to save some memory

    val meEpsilonV = delegate.addVertex(epsilonV)
    val hyperedges = new mutable.ArrayBuffer[HyperEdge[H,E]](hyperEdgeInfo.size)
    for (heInfo: (H, Seq[(PackedVertex[V],E)]) <- hyperEdgeInfo) {
      val (h, edgeInfo) = heInfo
      for ( (srcV, e) <- edgeInfo) assert(srcV != sink, "This meta-edge would create a cycle")
      val he = delegate.addHyperEdge(h, edgeInfo, meEpsilonV)
      hyperedges += he
    }

    // associate this epsilon vertex with its metaedge
    val me = new MetaEdge[M,H,E](meEpsilonV, m, hyperedges)
    metaEdgesByEpsilon += meEpsilonV -> me

    // delay adding the meta-edges to the delegate builder
    // until we know we've accumulated all of them
    metaEdgeSinks.getOrElseUpdate(sink, {new mutable.ArrayBuffer[PackedVertex[V]]}) += meEpsilonV
    
    return me
  }

  def build() = {
    // add single hyperedge that represents all incoming meta-edges
    // for all non-epsilon vertices
    val epsilonEdges = new mutable.HashSet[HyperEdge[H,E]]
    for( (sink, epsilonVertices) <- metaEdgeSinks) {
      val epsilonParents = for(v <- epsilonVertices) yield (v, epsilonE)
      val he = delegate.addHyperEdge(epsilonH, epsilonParents, sink)
      epsilonEdges += he
    }

    new MetaHyperDag[V,M,H,E](delegate.build, metaEdgesByEpsilon, epsilonEdges, phantomVertices)
  }
}
