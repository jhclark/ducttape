package ducttape.hyperdag.walker

import collection._

import ducttape.hyperdag._
import ducttape.hyperdag.meta.MetaHyperDag
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.util.MultiSet

/** our only job is to hide epsilon vertices during iteration
 *  see UnpackedDagWalker for definitions of filter and state types
 *  F is the FilterState */
class UnpackedMetaDagWalker[V,M,H,E,D,F](
        val dag: MetaHyperDag[V,M,H,E],
        val selectionFilter: SelectionFilter[D] = new DefaultSelectionFilter[D],
        val hedgeFilter: HyperEdgeFilter[H,E] = new DefaultHyperEdgeFilter[H,E],
        val constraintFilter: ConstraintFilter[V,D,F] = new DefaultConstraintFilter[V,D,F],
        val vertexFilter: MetaVertexFilter[V,H,E,D] = new DefaultMetaVertexFilter[V,H,E,D],
        val comboTransformer: ComboTransformer[H,E,D] = new DefaultComboTransformer[H,E,D],
        val toD: H => D = new DefaultToD[H])
  extends Walker[UnpackedMetaVertex[V,H,E,D]] {
  
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

  private val delegate = new UnpackedDagWalker[V,H,E,D,F](dag.delegate, selectionFilter, hedgeFilter, constraintFilter,
                                                          new DefaultVertexFilter[V,H,E,D], comboTransformer, toD)

  // we must be able to recover the epsilon-antecedents of non-epsilon vertices
  // ...AND phantom-antecedents to support graph-internal phantom vertices
  // so that we can properly populate their state maps
  // unfortunately, this kills space complexity, since we must hold on to these until
  // all members of a packed vertex have been unpacked
  // TODO: Right now, we don't dectect when all members of a packed vertex have
  // been unpacked so that we can reclaim space. Could we refcount them?
  private val skips = new mutable.HashMap[(PackedVertex[V],Seq[D]), UnpackedVertex[V,H,E,D]]
  
  // after getNextMeta skips over any phantoms and epsilons, we must then
  // re-map the parent realizations of the metavertex to be returned, which involves
  // tracing backward over any epsilon and phantom vertices
  private def followPhantomChain(prev: Seq[PackedVertex[V]]): Seq[PackedVertex[V]] = prev match {
    case Nil => Nil
    case Seq(single) if (dag.shouldSkip(single)) => followPhantomChain(dag.delegate.parents(single))
    case multiple => {
      if (multiple.exists(v => dag.shouldSkip(v))) {
        throw new RuntimeException("Found multiple parents while re-mapping parents of a meta vertex. " +
        		"However, one of those parents is an epsilon or phantom vertex.")
      }
      multiple // good, we've found our final parents
    }
  }
  
  private def mapParents(raw: UnpackedVertex[V,H,E,D]): Option[UnpackedMetaVertex[V,H,E,D]] = {  
    // TODO: CAN WE STOP UNPACKING EARLY?
    
    val parents: Seq[PackedVertex[V]] = followPhantomChain(Seq(raw.packed))
    assert(parents.size == raw.parentRealizations.size,
           "Parent size %d != parentReal.size %d".format(parents.size, raw.parentRealizations.size))
           
    // these lists are both parallel to the number of incoming metaedges
    val activeEdges = new mutable.ListBuffer[HyperEdge[H,E]]
    val metaParentReals = new mutable.ListBuffer[Seq[Seq[D]]]
           
    // for: parallel to number of incoming meta edges
    for ( (parentEpsilonV: PackedVertex[_], parentEpsilonReals: Seq[Seq[D]]) <- parents.zip(raw.parentRealizations)) {
      val parentEpsilonUV: UnpackedVertex[V,H,E,D] = skips( (parentEpsilonV, parentEpsilonReals) )
      // use this Seq[Seq[D]], which is parallel to the active hyperedge
      activeEdges += parentEpsilonUV.edge.get
      metaParentReals += parentEpsilonUV.parentRealizations
    }
    Some(new UnpackedMetaVertex[V,H,E,D](raw.packed, activeEdges, raw.realization, metaParentReals, raw))
  }
  
  private def getNextDelegate(): Option[UnpackedVertex[V,H,E,D]] = delegate.take() match {
    case Some(uv) if (dag.shouldSkip(uv.packed)) => {
      //println("TAKE SKIPPING: " + result)
      delegate.complete(uv)
      if (dag.shouldSkip(uv.packed)) {
        skips += (uv.packed, uv.realization) -> uv // TODO: We'd really prefer not to store these...
      }
      getNextDelegate()
    }
    case opt => opt
  }
  
  private def getNextMeta(): Option[UnpackedMetaVertex[V,H,E,D]] = getNextDelegate() match {
    case None => None
    case Some(raw: UnpackedVertex[_,_,_,_]) => mapParents(raw)
  }
  
  override def complete(item: UnpackedMetaVertex[V,H,E,D], continue: Boolean = true) = {
    delegate.complete(item.dual, continue)
  }

  override def take(): Option[UnpackedMetaVertex[V,H,E,D]] = getNextMeta() match {
    case Some(candidate) if (!vertexFilter(candidate)) => {
      complete(candidate, continue=false)
      take()
    }
    case opt => opt 
  }
}