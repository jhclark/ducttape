package ducttape.hyperdag.meta

import collection._
import ducttape.util._
import ducttape.hyperdag._
import ducttape.hyperdag.walker.PackedMetaDagWalker
import ducttape.hyperdag.walker.UnpackedMetaDagWalker

import ducttape.hyperdag.walker.UnpackedDagWalker._
import ducttape.hyperdag.walker._

// an implementation of MetaHyperDAGs based on transforming
// meta-edges into epsilon vertices (but these are hidden from the user)

// immutable
//
// walker will note when epsilon vertices are completed, but not actually
// return them to the user
//
// TODO: Pass filters to dag walker
class MetaHyperDag[V,M,H,E](private[hyperdag] val delegate: HyperDag[V,H,E],
                            private[hyperdag] val metaEdgesByEpsilon: Map[PackedVertex[V],MetaEdge[M,H,E]],
                            private[hyperdag] val epsilonEdges: Set[HyperEdge[H,E]],
                            private[hyperdag] val phantomVertices: Set[PackedVertex[V]]) {

  // don't include epsilon vertices
  val size: Int = delegate.size - metaEdgesByEpsilon.size - phantomVertices.size

  private[hyperdag] def isEpsilon(v: PackedVertex[V]) = metaEdgesByEpsilon.contains(v)
  private[hyperdag] def isPhantom(v: PackedVertex[V]) = phantomVertices.contains(v)
  private[hyperdag] def shouldSkip(v: PackedVertex[V]) = isEpsilon(v) || isPhantom(v)
  private[hyperdag] def isEpsilon(h: HyperEdge[H,E]) = epsilonEdges(h)

  def packedWalker() = new PackedMetaDagWalker[V](this) // TODO: Exclude epsilons from completed, etc.

  def unpackedWalker[D,F](constraintFilter: ConstraintFilter[V,D,F] = new DefaultConstraintFilter[V,D,F],
                          vertexFilter: MetaVertexFilter[V,H,E,D] = new DefaultMetaVertexFilter[V,H,E,D],
                          comboTransformer: ComboTransformer[H,E,D] = new DefaultComboTransformer[H,E,D],
                          toD: H => D = new DefaultToD[H]) = {
    // TODO: Combine this hedgeFilter with an external one?
    // TODO: Allow filtering baseline from realizations
    // TODO: Exclude epsilons from completed, etc.
    // TODO: Map epsilons and phantoms for constraintFiler in this class instead of putting
    // the burden on the filter
    val epsilonHyperEdgeFilter = new HyperEdgeFilter[H,E] {
      override def apply(he: HyperEdge[H,E]) = !isEpsilon(he)
    }

    new UnpackedMetaDagWalker[V,M,H,E,D,F](this, new DefaultSelectionFilter[D], epsilonHyperEdgeFilter,
                                           constraintFilter, vertexFilter, comboTransformer, toD)
  }

  def inMetaEdges(v: PackedVertex[V]): Seq[MetaEdge[M,H,E]]
    = for(parent <- delegate.parents(v)) yield metaEdgesByEpsilon(parent)
  def inHyperEdges(me: MetaEdge[M,H,E]): Seq[HyperEdge[H,E]]
    = delegate.inEdgesMap.getOrElse(me.epsilonV, Seq.empty)
  def outHyperEdges(v: PackedVertex[V]): Seq[HyperEdge[H,E]]
    = delegate.outEdgesMap.getOrElse(v, Seq.empty)
  def outMetaEdge(he: HyperEdge[H,E]): MetaEdge[M,H,E] = {
    return metaEdgesByEpsilon(delegate.sink(he))
  }

  private def skipEpsilonsAndPhantom(v: PackedVertex[_],
                                     func: PackedVertex[_] => Seq[PackedVertex[V]])
    : Seq[PackedVertex[V]] = {

    import System._
    var directParents = func(v)
    // check if we can just return these parents without modification
    if(directParents.exists(p => shouldSkip(p))) {
      // replace the epsilon vertices by their parents
      // it's guaranteed that those parents are not epsilon vertices themselves
      // TODO: This could be made into an ArrayBuffer if this turns out to be inefficient
      directParents.flatMap({
        case p if(isEpsilon(p)) => func(p).filter(!isPhantom(_)) // skip any phantom grandparents
        case p if(isPhantom(p)) => Seq.empty // skip entirely
        case p => Seq(p) // direct parent is normal
      })
    } else {
      directParents
    }    
  }

  def vertices() = delegate.vertices.filter(!shouldSkip(_))
  def parents(v: PackedVertex[_]): Seq[PackedVertex[V]] = skipEpsilonsAndPhantom(v, delegate.parents)
  def children(v: PackedVertex[_]): Seq[PackedVertex[V]] = skipEpsilonsAndPhantom(v, delegate.children)
  
  def sources(e: MetaEdge[M,H,E]): Seq[PackedVertex[V]] = {
    val srcs = new mutable.ArrayBuffer[PackedVertex[V]]
    for(inEdge <- inHyperEdges(e)) {
      srcs ++= delegate.sources(inEdge)
    }
    srcs
  }
  def sources(e: HyperEdge[H,E]): Seq[PackedVertex[V]] = delegate.sources(e)
  def sink(e: HyperEdge[H,E]): PackedVertex[V] = sink(outMetaEdge(e))
  def sink(e: MetaEdge[M,H,E]): PackedVertex[V] = delegate.children(e.epsilonV).head

  def toGraphViz(): String = delegate.toGraphViz(vertices, parents, {v => v.toString})

  // visualize with all epsilon and phantom vertices
  def toGraphVizDebug(): String = {
    def stringify(v: PackedVertex[V]): String = v match {
        case p if isPhantom(p) => "Phantom#" + p.id
        case p if isEpsilon(p) => "Epsilon:" + metaEdgesByEpsilon(p).m.toString + "#" + p.id
        case p => p.toString
    }
    delegate.toGraphViz(delegate.vertices, delegate.parents, stringify)
  }
}

