package ducttape.hyperdag.walker

import collection._
import ducttape.hyperdag._
import ducttape.hyperdag.meta.MetaHyperDag
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.util.MultiSet
import grizzled.slf4j.Logging
import annotation.tailrec

/** our only job is to hide epsilon vertices during iteration
 *  see UnpackedDagWalker for definitions of filter and state types
 * 
 * see [[ducttape.hyperdag.meta.MetaHyperDag]] for definitions of generic types V,M,H,E
 * see [[ducttape.hyperdag.walker.UnpackedDagWalker]] for definitions of generic types D,F
 */

class UnpackedMetaDagWalker[V,M,H,E,D,F](
    val dag: MetaHyperDag[V,M,H,E],
    munger: RealizationMunger[V,H,E,D,F],
    vertexFilter: MetaVertexFilter[V,H,E,D] = new DefaultMetaVertexFilter[V,H,E,D],
    toD: H => D = new DefaultToD[H],
    traversal: Traversal = Arbitrary,
    observer: UnpackedVertex[V,H,E,D] => Unit = (v: UnpackedVertex[V,H,E,D]) => { ; } )
   (implicit ordering: Ordering[D])
  extends Walker[UnpackedMetaVertex[V,H,E,D]] with Logging {

  /** get an exact replica of this walker, but starting the traversal over again */
  def duplicate(): UnpackedMetaDagWalker[V,M,H,E,D,F] = {
    new UnpackedMetaDagWalker(dag, munger, vertexFilter, toD, traversal, observer)
  }
  
  // never allow children to transform epsilons
  // and remove epsilon hyperedges from derivation
  object MetaRealizationMunger extends RealizationMunger[V,H,E,D,F] {
    
    override def initHyperedge(dOpt: Option[D]) = munger.initHyperedge(dOpt)
    override def toRealization(state: F) = munger.toRealization(state)
    
    private val emptyState = initHyperedge(None)
    
    override def finishHyperedge(v: PackedVertex[V], heOpt: Option[HyperEdge[H,E]], state: F) = heOpt match {
      case None => munger.finishHyperedge(v, heOpt, state)
      case Some(he: HyperEdge[_,_]) => {
        if (dag.isEpsilon(he)) {
          Some(state) // never allow child mungers to transform/see epsilons
        } else {
          munger.finishHyperedge(v, heOpt, state)
        }
      }
    }
    
    override def beginHyperedge(v: PackedVertex[V], heOpt: Option[HyperEdge[H,E]], prevState: F): Option[F] = {
      heOpt match {
        case None => Some(emptyState)
        case Some(he) => {
          if (dag.isEpsilon(he))
            Some(emptyState)
          else
            Some(prevState)
        }
      }
    }
  }

  // it's important that we call our parent's vertex filter here
  // so that traversal indexing can still run in a reasonable amount of time
  object VertexFilterAdapter extends VertexFilter[V,H,E,D] {
    override def apply(uv: UnpackedVertex[V,H,E,D]) = {
      // Note: We'd really rather not make this call (see toUnpackedMetaVertex)
      observer(uv)
      if (dag.isEpsilon(uv.packed)) {
        rememberEpsilon(uv)
        true // hide epsilons from the user, but don't actually filter them out
      } else {
        // if it's not an epsilon, we should ask the user filter whether or not to skip it
        val candidate = toUnpackedMetaVertex(uv)
        vertexFilter(candidate)
      }
    }
  }

  // NOTE: ORDER IS IMPORTANT -- epsilons MUST be initialized before delegate
  // since traversal vertex indexing relies on epsilons via the VertexFilterAdapter
  // we must be able to recover the epsilon-antecedents of non-epsilon vertices
  // so that we can properly populate their state maps
  // unfortunately, this kills space complexity, since we must hold on to these until
  // all members of a packed vertex have been unpacked
  // TODO: Right now, we don't dectect when all members of a packed vertex have
  // been unpacked so that we can reclaim space. Could we refcount them?
  private val epsilons = new mutable.HashMap[(PackedVertex[V],Seq[D]), UnpackedVertex[V,H,E,D]]

  // TODO: private.
  val delegate = new UnpackedDagWalker[V,H,E,D,F](
    dag.delegate, MetaRealizationMunger.andThen(munger), VertexFilterAdapter, toD, traversal)

  override def complete(item: UnpackedMetaVertex[V,H,E,D], continue: Boolean = true) = {
    debug(s"Completing: ${item}")
    delegate.complete(item.dual, continue)
  }

  private def rememberEpsilon(uv: UnpackedVertex[V,H,E,D]) {
    // TODO: We'd really prefer not to store these...
    epsilons.synchronized {
        epsilons += (uv.packed, uv.realization) -> uv
    }
  }

  // ideally, this would only ever get called once inside getNext()
  // however, to make sure that duplicate() inside our UnpackedDagWalker delegate
  // still runs in a reasonable amount of time while calculating traversal indices for depth/breadth first
  // we must also call it while evaluating the VertexFilter
  private def toUnpackedMetaVertex(raw: UnpackedVertex[V,H,E,D]): UnpackedMetaVertex[V,H,E,D] = {
    trace(s"Begin unpacking new meta vertex: ${raw}")
    assert(raw != null)

    val parents = dag.delegate.parents(raw.packed)
    assert(parents != null)
    assert(raw.parentRealizations != null)
    assert(parents.size == raw.parentRealizations.size,
           s"Parent size ${parents.size} != parentReal.size ${raw.parentRealizations.size}")
    
    // these lists are all parallel to the number of incoming metaedges
    assert(epsilons != null)
    val unpackedParents: Seq[UnpackedVertex[V,H,E,D]] = epsilons.synchronized {
      assert(parents != null)
      assert(raw != null)
      assert(raw.parentRealizations != null)
      val x = parents.zip(raw.parentRealizations)
      assert(x != null)
      x.map { case (parentEpsilonV, parentEpsilonReals) =>
        assert(parentEpsilonV != null)
        assert(parentEpsilonReals != null)
        epsilons( (parentEpsilonV, parentEpsilonReals) )
      }
    }
    val activeEdges: Seq[HyperEdge[H,E]] = unpackedParents.map(unpacked => unpacked.edge.get)
    val metaParentReals: Seq[Seq[Seq[D]]] = unpackedParents.map(unpacked => unpacked.parentRealizations)
    new UnpackedMetaVertex[V,H,E,D](raw.packed, activeEdges, raw.realization, metaParentReals, raw)
  }
  
  private def getNext(): Option[UnpackedMetaVertex[V,H,E,D]] = {
    
    // never return epsilon vertices
    @tailrec def takeSkippingEpsilons(): Option[UnpackedVertex[V,H,E,D]] = delegate.take() match {
      case None => None
      case result @ Some(uv) => {
        if (dag.shouldSkip(uv.packed)) {
          debug(s"Skipping: ${uv}")
          if (dag.isEpsilon(uv.packed)) {
            rememberEpsilon(uv)
          }
          delegate.complete(uv)
          takeSkippingEpsilons()
        } else {
          debug(s"Took non-epsilon vertex: ${uv}")
          result
        }
      }
    }
      
    takeSkippingEpsilons() match {
      case None => None
      case Some(raw: UnpackedVertex[_,_,_,_]) => Some(toUnpackedMetaVertex(raw))
    }
  }

  // the MetaVertexFilter is now evaluated by a call from our delegate to VertexFilterAdapter
  override def take(): Option[UnpackedMetaVertex[V,H,E,D]] = getNext()
}
