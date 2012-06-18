package ducttape.hyperdag.walker

import collection._
import ducttape.hyperdag.PhantomHyperDag
import ducttape.hyperdag.UnpackedVertex
import ducttape.util.MultiSet
import ducttape.hyperdag.PackedVertex
import grizzled.slf4j.Logging
import ducttape.hyperdag.meta.PhantomMetaHyperDag
import ducttape.hyperdag.meta.UnpackedChainedMetaVertex
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.hyperdag.HyperEdge
import scala.annotation.tailrec

// NOTE: Root/terminal phantom vertices behave very different than internal phantom vertices
//       perhaps they deserve different names or special treatment otherwise?
// TODO: Create ChainedUnpackedVertex and return those instead
class UnpackedPhantomMetaDagWalker[V,M,H,E,D,F](
        val dag: PhantomMetaHyperDag[V,M,H,E],
        selectionFilter: SelectionFilter[D] = new DefaultSelectionFilter[D],
        hedgeFilter: HyperEdgeFilter[H,E] = new DefaultHyperEdgeFilter[H,E],
        constraintFilter: ConstraintFilter[Option[V],H,E,D,F] = new DefaultConstraintFilter[Option[V],H,E,D,F],
        vertexFilter: MetaVertexFilter[Option[V],H,E,D] = new DefaultMetaVertexFilter[Option[V],H,E,D],
        comboTransformer: ComboTransformer[H,E,D] = new DefaultComboTransformer[H,E,D],
        toD: H => D = new DefaultToD[H],
        observer: UnpackedVertex[Option[V],H,E,D] => Unit = (v: UnpackedVertex[Option[V],H,E,D]) => { ; } )
       (implicit ordering: Ordering[D])
  extends Walker[UnpackedChainedMetaVertex[V,H,E,D]] with Logging {
  
  object MetaVertexFilterAdapter extends MetaVertexFilter[Option[V],H,E,D] {
    override def apply(v: UnpackedMetaVertex[Option[V],H,E,D]) = v.packed.value match {
      case Some(_) => vertexFilter(v)
      case None => true
    }
  }
  
  val delegate = new UnpackedMetaDagWalker[Option[V],M,H,E,D,F](
    dag.delegate, selectionFilter, hedgeFilter, constraintFilter, MetaVertexFilterAdapter, comboTransformer, toD, observer)
    
  // we must be able to recover the phantom-antecedents of non-phantom vertices
  // so that we can properly populate their state maps
  // unfortunately, this kills space complexity, since we must hold on to these until
  // all members of a packed vertex have been unpacked
  // TODO: Right now, we don't dectect when all members of a packed vertex have
  // been unpacked so that we can reclaim space. Could we refcount them?
  private val unpackedMap = new mutable.HashMap[(PackedVertex[Option[V]],Seq[D]), UnpackedMetaVertex[Option[V],H,E,D]]
  
  private def getOnly[A](seq: Seq[A]): A = seq match {
    case Seq(only) => only
    case _ => throw new RuntimeException("phantom chains can only have single parents; " +
          "expected exactly one element in: " + seq)
  }

  // this is only ever called from takeSkippingPhantoms, where it is guaranteed to have a lock on unpackedMap
  // note: cannot be @tailrec
  private def followPhantomChain(v: UnpackedMetaVertex[Option[V],H,E,D], edge: E, parentReal: Seq[D])
                                : Seq[(E, Seq[D])] = {
    trace("Follow phantom chain at " + v)
    v.packed.value match {
      case Some(_) => Seq( (edge, parentReal) )
      case None => {
        trace("Zip: " + v.edges.zip(v.parentRealizations))

        // use active hyperedges to find parents
        val parents: Seq[(E,UnpackedMetaVertex[Option[V],H,E,D])]
          = v.edges.zip(v.parentRealizations).
              flatMap { case (hyperedge, parentReals) => {
                import ducttape.util.Collections._
                zip3(dag.delegate.sources(hyperedge), hyperedge.e, parentReals).map { case (parent, e, parentReal) =>
                  // parent: PackedVertex[Option[V]]
                  trace("Resolving hyperedge %s: parent is %s".format(hyperedge, parent))
                  val uv: UnpackedMetaVertex[Option[V],H,E,D] = unpackedMap( (parent, parentReal.sorted(ordering)) )
                  (e, uv)
                }
              }
            }
        trace("Parents of %s are: %s".format(v, parents))
        parents match {
          case Seq() => Seq( (edge, parentReal) ) // this is a leaf/terminal phantom vertex
          case _ => {
            parents.flatMap { case (e, unpackedV) =>
              followPhantomChain(unpackedV, e, unpackedV.realization)
            }
          }
        }
      }
    }
  }
  
  @tailrec
  private def takeSkippingPhantoms(): Option[UnpackedChainedMetaVertex[V,H,E,D]] = delegate.take() match {
    case None => None
    case Some(umv) => umv.packed.value match {
      case Some(packed) => {
        trace("Begin unpacking chained meta vertex: " + umv)
        
        // we can have phantom vertex chains of arbitrary length
        // epsilons are already removed by our delegate
        import ducttape.util.Collections._
        val parentInfo: Seq[(Seq[E], Seq[Seq[D]])] = unpackedMap.synchronized {
          zip3(umv.edges, umv.parentRealizations, umv.edges).
          map { case (hyperedge, parentReals, hyperEdge) =>
            val munged: Seq[(E, Seq[D])] = zip3(dag.delegate.sources(hyperedge), parentReals, hyperedge.e) flatMap {
              case (parent, parentReal, edge) => {
                trace("Begin backtracing phantom chain for " + parent)
                // TODO: DON'T RE-SORT HERE
                val unpackedV = unpackedMap( (parent, parentReal.sorted(ordering)) )
                val leafParents: Seq[(E, Seq[D])] = followPhantomChain(unpackedV, edge, parentReal)
                leafParents
              }
            }
            
            val finalEdges = munged.map(_._1)
            val finalParentReals = munged.map(_._2)
            (finalEdges, finalParentReals)
          }
        }
        val mungedEdges: Seq[Seq[E]] = parentInfo.map(_._1)
        val mungedParentReals: Seq[Seq[Seq[D]]] = parentInfo.map(_._2)
        
        val parentsSize = parentInfo.size
        assert(parentsSize == umv.edges.size)
        assert(parentsSize == umv.parentRealizations.size,
               "Parent size %d != parentReal.size %d".format(parentsSize, umv.parentRealizations.size))
        assert(parentsSize == mungedParentReals.size)

        debug("Yielding: " + umv)
        Some(new UnpackedChainedMetaVertex[V,H,E,D](umv.packed, mungedEdges, umv.realization, mungedParentReals, umv)) 
      }
      case None => {
        // phantom: save for later
        debug("Phantom skipping: " + umv)
        unpackedMap.synchronized {
          unpackedMap += (umv.packed, umv.realization.sorted(ordering)) -> umv
        }
        delegate.complete(umv)
        takeSkippingPhantoms()
      }
    }
  }
  
  override def take(): Option[UnpackedChainedMetaVertex[V,H,E,D]] = takeSkippingPhantoms()

  override def complete(item: UnpackedChainedMetaVertex[V,H,E,D], continue: Boolean = true) = {
    debug("Completing " + item)
    unpackedMap.synchronized {
      unpackedMap += (item.packed, item.realization.seq.sorted(ordering)) -> item.dual
    }
    delegate.complete(item.dual, continue)
  }
}
