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
    delegate.complete(item.dual, continue)
  }

  override def take(): Option[UnpackedMetaVertex[V,H,E,D]] = {

    def getNext(): Option[UnpackedMetaVertex[V,H,E,D]] = {
      var result: Option[UnpackedVertex[V,H,E,D]] = delegate.take()
      // never return epsilon vertices nor phantom verties
      // we're guaranteed to only have one epsilon vertex in between vertices (no chains)
      // but phantom vertices break this
      while (!result.isEmpty && dag.shouldSkip(result.get.packed)) {
        //println("TAKE SKIPPING: " + result)
        val uv = result.get
        delegate.complete(uv)
        if (dag.isEpsilon(uv.packed)) {
          // TODO: We'd really prefer not to store these...
          epsilons += (uv.packed, uv.realization) -> uv
        }
        result = delegate.take()
      }
      //println("TAKING: " + result)
      
      return result match {
        case None => None
        case Some(raw: UnpackedVertex[_,_,_,_]) => {
          
          // TODO: CAN WE STOP UNPACKING EARLY?
          
          // these lists are both parallel to the number of incoming metaedges
          val activeEdges = new mutable.ListBuffer[HyperEdge[H,E]]
          val metaParentReals = new mutable.ListBuffer[Seq[Seq[D]]]
          
          def followPhantomChain(prev: Seq[PackedVertex[V]]): Seq[PackedVertex[V]] = {
            dag.delegate.parents(v) match {
                case Nil => None // skip the case of phantom parents if phantom has in-degree zero
                case Seq(singleParent) if (dag.isPhantom(singleParent)) => { // get *its* parent if it has in-degree == 1
                  // this parent will be an epsilon -- then get *ITS* parent
                  followPhantomChain(singleParent)
                }
                case _ => {
                  // phantom vertices with in-degree > 1 are disallowed (to maintain constraint that all metaedges are active)
                  throw new RuntimeException("Phantom vertices with in-degree > 1 are not allowed")
                }
              }
          }
          
          val directParents: Seq[PackedVertex[V]] = dag.delegate.parents(raw.packed)
          val parents: Seq[PackedVertex[V]] = followPhantomChain(directParents)
          assert(parents.size == raw.parentRealizations.size,
                 "Parent size %d != parentReal.size %d".format(parents.size, raw.parentRealizations.size))
          // for: parallel to number of incoming meta edges
          for ( (parentEpsilonV: PackedVertex[_], parentEpsilonReals: Seq[Seq[D]]) <- parents.zip(raw.parentRealizations)) {
            val parentEpsilonUV: UnpackedVertex[V,H,E,D] = epsilons( (parentEpsilonV, parentEpsilonReals) )
            // use this Seq[Seq[D]], which is parallel to the active hyperedge
            activeEdges += parentEpsilonUV.edge.get
            metaParentReals += parentEpsilonUV.parentRealizations
          }
          Some(new UnpackedMetaVertex[V,H,E,D](raw.packed, activeEdges, raw.realization, metaParentReals, raw))
        }
      }
    } // getNext
      
    var result: Option[UnpackedMetaVertex[V,H,E,D]] = getNext()
    while (result != None && !vertexFilter(result.get)) {
      //System.err.println("MEAT Vertex filter does not contain: " + result.get)
      complete(result.get, continue=false)
      result = getNext()
    }
    result
  } // take
}