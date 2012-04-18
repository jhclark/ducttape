package ducttape.hyperdag.walker

import collection._

import ducttape.hyperdag._
import ducttape.hyperdag.meta.MetaHyperDag
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.util.MultiSet

import UnpackedDagWalker._

object UnpackedMetaDagWalker {
  trait MetaVertexFilter[V,H,E] {
    def apply(v: UnpackedMetaVertex[V,H,E]): Boolean
  }
  def DefaultMetaVertexFilter[V,H,E] = new MetaVertexFilter[V,H,E] {
    override def apply(v: UnpackedMetaVertex[V,H,E]) = true
  }
}

import UnpackedMetaDagWalker._

/** our only job is to hide epsilon vertices during iteration
 *  see UnpackedDagWalker for definitions of filter and state types
 *  F is the FilterState */
class UnpackedMetaDagWalker[V,M,H,E,F](val dag: MetaHyperDag[V,M,H,E],
        val selectionFilter: SelectionFilter[H] = DefaultSelectionFilter,
        val hedgeFilter: HyperEdgeFilter[H,E] = DefaultHyperEdgeFilter,
        val constraintFilter: ConstraintFilter[V,H,F] = DefaultConstraintFilter,
        val vertexFilter: MetaVertexFilter[V,H,E] = DefaultMetaVertexFilter,
        val comboTransformer: ComboTransformer[H] = DefaultComboTransformer)
  extends Walker[UnpackedMetaVertex[V,H,E]] {

  private val delegate = new UnpackedDagWalker[V,H,E,F](dag.delegate, selectionFilter, hedgeFilter,
                                                        constraintFilter, DefaultVertexFilter, comboTransformer)

  // we must be able to recover the epsilon-antecedents of non-epsilon vertices
  // so that we can properly populate their state maps
  // unfortunately, this kills space complexity, since we must hold on to these until
  // all members of a packed vertex have been unpacked
  // TODO: Right now, we don't dectect when all members of a packed vertex have
  // been unpacked so that we can reclaim space. Could we refcount them?
  private val epsilons = new mutable.HashMap[(PackedVertex[V],Seq[H]), UnpackedVertex[V,H,E]]

  override def complete(item: UnpackedMetaVertex[V,H,E], continue: Boolean = true) = {
    delegate.complete(item.dual, continue)
  }

  override def take(): Option[UnpackedMetaVertex[V,H,E]] = {

    def getNext(): Option[UnpackedMetaVertex[V,H,E]] = {
      var result: Option[UnpackedVertex[V,H,E]] = delegate.take()
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
        case Some(raw: UnpackedVertex[_,_,_]) => {
          
          // TODO: CAN WE STOP UNPACKING EARLY?
          
          val activeEdges = new mutable.ListBuffer[HyperEdge[H,E]]
          val metaParentReals = new mutable.ListBuffer[Seq[Seq[H]]]
          
          dag.delegate.parents(raw.packed) match {
            // skip the case of phantom parents
            case Seq(singleParent) if(dag.isPhantom(singleParent)) => ;
            case parents => {
              assert(parents.size == raw.parentRealizations.size, "Parent size %d != parentReal.size %d".format(parents.size, raw.parentRealizations.size))
              // for parallel to number of incoming meta edges
              for ((parentEpsilonV: PackedVertex[V], parentEpsilonReals: Seq[Seq[H]]) <- parents.zip(raw.parentRealizations)) {
                val parentEpsilonUV: UnpackedVertex[V,H,E] = epsilons( (parentEpsilonV, parentEpsilonReals) )
                // use this Seq[Seq[H]], which is parallel to the active hyperedge
                activeEdges += parentEpsilonUV.edge.get
                metaParentReals += parentEpsilonUV.parentRealizations
              }
            }
          }
          Some(new UnpackedMetaVertex[V,H,E](raw.packed, activeEdges, raw.realization, metaParentReals, raw))
        }
      }
    } // getNext
      
    var result: Option[UnpackedMetaVertex[V,H,E]] = getNext()
    while (result != None && !vertexFilter(result.get)) {
      //System.err.println("MEAT Vertex filter does not contain: " + result.get)
      complete(result.get, continue=false)
      result = getNext()
    }
    result
  } // take
}