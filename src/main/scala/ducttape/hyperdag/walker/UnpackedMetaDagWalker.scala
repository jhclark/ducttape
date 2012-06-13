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
 *  F is the FilterState */
class UnpackedMetaDagWalker[V,M,H,E,D,F](
    val dag: MetaHyperDag[V,M,H,E],
    selectionFilter: SelectionFilter[D] = new DefaultSelectionFilter[D],
    hedgeFilter: HyperEdgeFilter[H,E] = new DefaultHyperEdgeFilter[H,E],
    constraintFilter: ConstraintFilter[V,H,E,D,F] = new DefaultConstraintFilter[V,H,E,D,F],
    vertexFilter: MetaVertexFilter[V,H,E,D] = new DefaultMetaVertexFilter[V,H,E,D],
    comboTransformer: ComboTransformer[H,E,D] = new DefaultComboTransformer[H,E,D],
    toD: H => D = new DefaultToD[H])
  extends Walker[UnpackedMetaVertex[V,H,E,D]] with Logging {
  
  object MetaComboTransformer extends ComboTransformer[H,E,D] {
    override def apply(heOpt: Option[HyperEdge[H,E]], combo: MultiSet[D]) = heOpt match {
      case None => comboTransformer(heOpt, combo)
      case Some(he: HyperEdge[_,_]) => {
        if (dag.isEpsilon(he)) {
          Some(combo) // never transform epsilons
        } else {
          comboTransformer(heOpt, combo)
        }
      }
    }
  }
  
  // TODO: XXX: Combine with hedgeFilter from above
  object EpsilonHyperEdgeFilter extends HyperEdgeFilter[H,E] { // TODO: Rename as "mask"
    override def apply(he: HyperEdge[H,E]) = !dag.isEpsilon(he)
  }

  private val delegate = new UnpackedDagWalker[V,H,E,D,F](
    dag.delegate, selectionFilter, hedgeFilter, constraintFilter,
    new DefaultVertexFilter[V,H,E,D], comboTransformer, toD)

  // we must be able to recover the epsilon-antecedents of non-epsilon vertices
  // so that we can properly populate their state maps
  // unfortunately, this kills space complexity, since we must hold on to these until
  // all members of a packed vertex have been unpacked
  // TODO: Right now, we don't dectect when all members of a packed vertex have
  // been unpacked so that we can reclaim space. Could we refcount them?
  private val epsilons = new mutable.HashMap[(PackedVertex[V],Seq[D]), UnpackedVertex[V,H,E,D]]

  override def complete(item: UnpackedMetaVertex[V,H,E,D], continue: Boolean = true) = {
    debug("Completing: " + item)
    delegate.complete(item.dual, continue)
  }
  
  private def getNext(): Option[UnpackedMetaVertex[V,H,E,D]] = {
    
    // never return epsilon vertices
    @tailrec def takeSkippingEpsilons(): Option[UnpackedVertex[V,H,E,D]] = delegate.take() match {
      case None => None
      case result @ Some(uv) => {
        if (dag.shouldSkip(uv.packed)) {
          debug("Skipping: " + uv)
          if (dag.isEpsilon(uv.packed)) {
            // TODO: We'd really prefer not to store these...
            epsilons.synchronized {
              epsilons += (uv.packed, uv.realization) -> uv
            }
          }
          delegate.complete(uv)
          takeSkippingEpsilons()
        } else {
          debug("Took non-epsilon vertex: " + uv)
          result
        }
      }
    }
      
    takeSkippingEpsilons() match {
      case None => None
      case Some(raw: UnpackedVertex[_,_,_,_]) => {
        trace("Begin unpacking new meta vertex: " + raw)

        val parents = dag.delegate.parents(raw.packed)
        assert(parents.size == raw.parentRealizations.size,
               "Parent size %d != parentReal.size %d".format(parents.size, raw.parentRealizations.size))

        // these lists are all parallel to the number of incoming metaedges
        val unpackedParents: Seq[UnpackedVertex[V,H,E,D]] = epsilons.synchronized {
            parents.zip(raw.parentRealizations).map {
            case (parentEpsilonV, parentEpsilonReals) => {
              epsilons( (parentEpsilonV, parentEpsilonReals) )
            }
          }
        }
        val activeEdges: Seq[HyperEdge[H,E]] = unpackedParents.map(unpacked => unpacked.edge.get)
        val metaParentReals: Seq[Seq[Seq[D]]] = unpackedParents.map(unpacked => unpacked.parentRealizations)
        val umv = new UnpackedMetaVertex[V,H,E,D](raw.packed, activeEdges, raw.realization, metaParentReals, raw)
        Some(umv)
      }
    }
  }

  override def take(): Option[UnpackedMetaVertex[V,H,E,D]] = {
    @tailrec def takeSkippingFiltered(): Option[UnpackedMetaVertex[V,H,E,D]] = getNext() match {
      case None => None
      case result @ Some(candidate) => { 
        if (vertexFilter(candidate)) {
          debug("Yielding: " + candidate)
          result
        } else {
          debug("META Vertex filter does not contain: " + candidate)
          complete(candidate, continue=false)
          takeSkippingFiltered() 
        }
      }
    }
    takeSkippingFiltered()
  }
}
