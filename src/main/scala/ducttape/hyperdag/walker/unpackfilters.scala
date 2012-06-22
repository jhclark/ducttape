package ducttape.hyperdag.walker

import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.UnpackedVertex
import ducttape.hyperdag.meta.UnpackedChainedMetaVertex

import collection._

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

  // 1) the initial state when we begin traversing each hyperedge
  // Note: d may be None if this vertex has no incoming hyperedge
  def initHyperedge(dOpt: Option[D]): F
  
  // 2) when we first encounter the hyperedge, we must add the payload that this hyperedge will introduce
  //    we might choose to ignore that payload (e.g. if a branch is from an epsilon vertex)
  def beginHyperedge(v: PackedVertex[V], heOpt: Option[HyperEdge[H,E]], prevState: F): Option[F]
    = Some(prevState)
  
  // 3) Called for each incoming component edge of the current hyperedge
  //    he is passed mainly for debugging
  def traverseEdge(v: PackedVertex[V], heOpt: Option[HyperEdge[H,E]], e: E, parentReal: Seq[D], prevState: F): Option[F]
    = Some(prevState)
  
  // 4) Finish hyperedges
  // when we finish traversing the hyperedge, we're free to accept it or not, but we should be done modifying realizations
  def finishHyperedge(v: PackedVertex[V], heOpt: Option[HyperEdge[H,E]], state: F): Option[F] = Some(state)

  // 5) convert efficient state back to a realization (it will be sorted by walker)
  def toRealization(state: F): Seq[D]

  // create a composite RealizationMunger (convenience method, not part of munging API)
  // init: which munger will be used to initialize the unpacking state?
  // end: which munger's toRealization() method will be used to convert the state back to a realization?
  def andThen(that: RealizationMunger[V,H,E,D,F]) = new CompositeRealizationMunger[V,H,E,D,F](this, that)()
}

// first: the first munger to run
// second: the munger to run after than
// init: which munger will be used to initialize the unpacking state?
// end: which munger's toRealization() method will be used to convert the state back to a realization?
class CompositeRealizationMunger[V,H,E,D,F](
    first: RealizationMunger[V,H,E,D,F],
    second: RealizationMunger[V,H,E,D,F])
   (init: RealizationMunger[V,H,E,D,F] = first,
    end: RealizationMunger[V,H,E,D,F] = second)
    extends RealizationMunger[V,H,E,D,F] {
  
  // some "fluent" methods for building a munger
  def withInit(newInit: RealizationMunger[V,H,E,D,F])
    = new CompositeRealizationMunger[V,H,E,D,F](first, second)(newInit, end)
    
  def withEnd(newEnd: RealizationMunger[V,H,E,D,F])
    = new CompositeRealizationMunger[V,H,E,D,F](first, second)(init, newEnd)
    
  override def toString() = "([init=%s] %s => %s [end=%s])".format(init, first, second, end)
  
  // really, more of an initState...
  override def initHyperedge(d: Option[D]): F = init.initHyperedge(d)

  override def beginHyperedge(v: PackedVertex[V], he: Option[HyperEdge[H,E]], prevState: F): Option[F] = {
    first.beginHyperedge(v, he, prevState) match {
      case None => None
      case Some(intermediate) => second.beginHyperedge(v, he, intermediate) 
    }
  }
  
  override def traverseEdge(v: PackedVertex[V], he: Option[HyperEdge[H,E]], e: E, parentRealization: Seq[D], prevState: F): Option[F] = {
    first.traverseEdge(v, he, e, parentRealization, prevState) match {
      case None => None
      case Some(intermediate) => second.traverseEdge(v, he, e, parentRealization, intermediate) 
    }
  }

  override def finishHyperedge(v: PackedVertex[V], he: Option[HyperEdge[H,E]], prevState: F): Option[F] = {
    first.finishHyperedge(v, he, prevState) match {
      case None => None
      case Some(intermediate) => second.finishHyperedge(v, he, intermediate) 
    }
  }

  override def toRealization(state: F) = end.toRealization(state)
}

trait DefaultRealizationStates[V,H,E,D] extends RealizationMunger[V,H,E,D,immutable.HashSet[D]] {
  override def initHyperedge(opt: Option[D]) = opt match {
    case None => new immutable.HashSet[D]
    case Some(d) => new immutable.HashSet[D] + d
  }
  override def toRealization(state: immutable.HashSet[D]): Seq[D] = state.toSeq
}

class DefaultRealizationMunger[V,H,E,D] extends DefaultRealizationStates[V,H,E,D];

