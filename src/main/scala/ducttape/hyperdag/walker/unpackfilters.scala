package ducttape.hyperdag.walker
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.util.MultiSet
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.UnpackedVertex
import ducttape.hyperdag.meta.UnpackedChainedMetaVertex

// these really better belong in companion objects
// for UnpackedDagWalker and UnpackedMetaDagWalker
// however, that caused inscrutable compiler errors
// for some bizarre reasons

trait MetaVertexFilter[V,H,E,D] {
  def apply(v: UnpackedMetaVertex[V,H,E,D]): Boolean
}

class DefaultMetaVertexFilter[V,H,E,D] extends MetaVertexFilter[V,H,E,D] {
  override def apply(v: UnpackedMetaVertex[V,H,E,D]) = true
}

trait VertexFilter[V,H,E,D] {
  def apply(v: UnpackedVertex[V,H,E,D]): Boolean
}

class DefaultVertexFilter[V,H,E,D] extends VertexFilter[V,H,E,D] {
  override def apply(v: UnpackedVertex[V,H,E,D]) = true
}

class DefaultToD[H] extends Function1[H,H] {
  override def apply(h: H) = h
}

// rename to TraversalMunger? or DerivationMunger?
trait RealizationMunger[V,H,E,D,F] {
  
  private var nada: F = _ // syntactic cruft, just to get a null...
  
  // 1) when we first encounter the hyperedge, we must add any realizations that this hyperedge will introduce
  def beginHyperedge(v: PackedVertex[V], he: Option[HyperEdge[H,E]], proposedReal: Seq[D]): Option[Seq[D]]
    = Some(proposedReal)

  // 2) Called just before our repeated calls to traverseEdge()
  //    this defines the initState for the state object passed through traverseEdge
  // TODO: Combine with beginHyperedge? Convert final state into Seq[D]?
  // There might be prevInitState if there are multiple components in a composite munger
  def beginEdges(v: PackedVertex[V], he: Option[HyperEdge[H,E]], initReal: Seq[D], prevInitState: Option[F] = None): Option[F] = {
    Some(nada)
  }
  
  // 3) Called for each incoming component edge of the current hyperedge
  //    he is passed mainly for debugging
  def traverseEdge(v: PackedVertex[V], he: Option[HyperEdge[H,E]], e: E, prevState: F, combo: MultiSet[D], parentRealization: Seq[D]): Option[F]
    = Some(prevState)
  
  // 4) Finish hyperedges
  // when we finish traversing the hyperedge, we're free to accept it or not, but we should be done modifying realizations
  // TODO: Make incoming multiset immutable!
  def finishHyperedge(v: PackedVertex[V], he: Option[HyperEdge[H,E]], combo: MultiSet[D]): Option[MultiSet[D]] = Some(combo)

  // create a composite RealizationMunger (convenience method, not part of munging API)
  def andThen(that: RealizationMunger[V,H,E,D,F]): RealizationMunger[V,H,E,D,F]
    = new CompositeRealizationMunger[V,H,E,D,F](this, that)
}

class CompositeRealizationMunger[V,H,E,D,F](
    first: RealizationMunger[V,H,E,D,F],
    second: RealizationMunger[V,H,E,D,F])
    extends RealizationMunger[V,H,E,D,F] {

  override def beginHyperedge(v: PackedVertex[V], he: Option[HyperEdge[H,E]], proposedReal: Seq[D]): Option[Seq[D]] = {
    first.beginHyperedge(v, he, proposedReal) match {
      case None => None
      case Some(intermediate) => second.beginHyperedge(v, he, intermediate) 
    }
  }
  
  // TODO: How do we initialize the state when we have multiple mungers?
  
  override def beginEdges(v: PackedVertex[V], he: Option[HyperEdge[H,E]], initReal: Seq[D], prevInitState: Option[F]): Option[F] = {
    first.beginEdges(v, he, initReal, prevInitState) match {
      case None => None
      case Some(intermediate) => second.beginEdges(v, he, initReal, Some(intermediate)) 
    }
  }
  
  override def traverseEdge(v: PackedVertex[V], he: Option[HyperEdge[H,E]], e: E, prevState: F, combo: MultiSet[D], parentRealization: Seq[D]): Option[F] = {
    first.traverseEdge(v, he, e, prevState, combo, parentRealization) match {
      case None => None
      case Some(intermediate) => second.traverseEdge(v, he, e, intermediate, combo, parentRealization) 
    }
  }

  override def finishHyperedge(v: PackedVertex[V], he: Option[HyperEdge[H,E]], combo: MultiSet[D]): Option[MultiSet[D]] = {
    first.finishHyperedge(v, he, combo) match {
      case None => None
      case Some(intermediate) => second.finishHyperedge(v, he, intermediate) 
    }
  }
}

class DefaultRealizationMunger[V,H,E,D,F] extends RealizationMunger[V,H,E,D,F];
