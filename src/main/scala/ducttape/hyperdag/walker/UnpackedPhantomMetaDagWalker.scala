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
        constraintFilter: ConstraintFilter[Option[V],D,F] = new DefaultConstraintFilter[Option[V],D,F],
        vertexFilter: MetaVertexFilter[Option[V],H,E,D] = new DefaultMetaVertexFilter[Option[V],H,E,D],
        comboTransformer: ComboTransformer[H,E,D] = new DefaultComboTransformer[H,E,D],
        toD: H => D = new DefaultToD[H])
  extends Walker[UnpackedChainedMetaVertex[V,H,E,D]] with Logging {
  
  object MetaVertexFilterAdapter extends MetaVertexFilter[Option[V],H,E,D] {
    override def apply(v: UnpackedMetaVertex[Option[V],H,E,D]) = v.packed.value match {
      case Some(_) => vertexFilter(v)
      case None => true
    }
  }
  
  val delegate = new UnpackedMetaDagWalker[Option[V],M,H,E,D,F](
    dag.delegate, selectionFilter, hedgeFilter, constraintFilter, MetaVertexFilterAdapter, comboTransformer, toD)
    
  // we must be able to recover the phantom-antecedents of non-phantom vertices
  // so that we can properly populate their state maps
  // unfortunately, this kills space complexity, since we must hold on to these until
  // all members of a packed vertex have been unpacked
  // TODO: Right now, we don't dectect when all members of a packed vertex have
  // been unpacked so that we can reclaim space. Could we refcount them?
  private val unpackedMap = new mutable.HashMap[(PackedVertex[Option[V]],Seq[D]), UnpackedMetaVertex[Option[V],H,E,D]]
  
  override def take(): Option[UnpackedChainedMetaVertex[V,H,E,D]] = recursiveTake()
  
  private def getOnly[A](seq: Seq[A]): A = seq match {
    case Seq(only) => only
    case _ => throw new RuntimeException("phantom chains can only have single parents; " +
          "expected exactly one element in: " + seq)
  }
  
  // TODO: XXX: HACK: Should we really have empty realizations floating around?
  private def getOnlySeq[A](seq: Seq[Seq[A]]): Seq[A] = seq match {
    case Seq(only) => only
    case _ => getOnly(seq.filter(!_.isEmpty))
  }
  
  @tailrec
  private def followPhantomChain(v: UnpackedMetaVertex[Option[V],H,E,D], edge: E, parentReal: Seq[D])
                                : (E, Seq[D]) = {
    trace("Follow phantom chain at " + v)
    v.packed.value match {
      case Some(_) => (edge, parentReal)
      case None => {
        trace("Zip: " + dag.delegate.parents(v.packed).zip(v.parentRealizations))

        // use active hyperedges to find parents
        val parents: Seq[UnpackedMetaVertex[Option[V],H,E,D]]
          = v.edges.zip(v.parentRealizations).
              flatMap { case (hyperedge, parentReals) => {
                dag.delegate.sources(hyperedge).map { parent: PackedVertex[Option[V]] =>
                  val parentReal = getOnlySeq(parentReals)
                  try {
                    val uv: UnpackedMetaVertex[Option[V],H,E,D] = unpackedMap( (parent, parentReal) )
                    uv
                  } catch {
                    case e => debug("Unpack map currently has: " + unpackedMap.keySet); throw e
                  }
                }
              }
            }
        trace("Parents of %s are: %s".format(v, parents))
        parents match {
          case Seq() => (edge, parentReal) // this is a root phantom vertex
          case Seq(singleUnpackedV) => {
            val myHyperEdge = getOnly(v.edges)
            val myParentReals = getOnlySeq(v.parentRealizations)
            val myEdge = getOnly(myHyperEdge.e)
            val myParentReal = getOnlySeq(myParentReals)
            followPhantomChain(singleUnpackedV, myEdge, myParentReal)
          }
          case _ => throw new RuntimeException("Found more than one parent of a phantom vertex")
        }
      }
    }
  }
  
  @tailrec
  private def recursiveTake(): Option[UnpackedChainedMetaVertex[V,H,E,D]] = delegate.take() match {
    case None => None
    case Some(umv) => umv.packed.value match {
      case Some(packed) => {
        // we can have phantom vertex chains of arbitrary length
        // epsilons are already removed by our delegate
        import ducttape.util.Collections._
        val parentInfo: Seq[(Seq[E], Seq[Seq[D]])]
          = zip3(umv.edges, umv.parentRealizations, umv.edges).
            map { case (hyperedge, parentReals, hyperEdge) =>
              val munged: Seq[(E, Seq[D])] = zip3(dag.delegate.sources(hyperedge), parentReals, hyperedge.e) map {
                case (parent, parentReal, edge) => {
                  trace("Begin backtracing phantom chain for " + parent)
                  val unpackedV = unpackedMap( (parent, parentReal) )
                  val (finalEdge, finalParentReal) = followPhantomChain(unpackedV, edge, parentReal)
                  (finalEdge, finalParentReal)
                }
              }
              
              val finalEdges = munged.map(_._1)
              val finalParentReals = munged.map(_._2)
              (finalEdges, finalParentReals)
            }
        val mungedEdges: Seq[Seq[E]] = parentInfo.map(_._1)
        val mungedParentReals: Seq[Seq[Seq[D]]] = parentInfo.map(_._2)
        
        val parentsSize = parentInfo.size
        assert(parentsSize == umv.edges.size)
        assert(parentsSize == umv.parentRealizations.size,
               "Parent size %d != parentReal.size %d".format(parentsSize, umv.parentRealizations.size))
        assert(parentsSize == mungedParentReals.size)

        debug("Yielding: " + umv.packed)
        Some(new UnpackedChainedMetaVertex[V,H,E,D](umv.packed, mungedEdges, umv.realization, mungedParentReals, umv)) 
      }
      case None => {
        // phantom: save for later
        debug("Phantom skipping: " + umv)
        delegate.complete(umv)
        unpackedMap += (umv.packed, umv.realization) -> umv
        recursiveTake()
      }
    }
  }
  
  override def complete(item: UnpackedChainedMetaVertex[V,H,E,D], continue: Boolean = true) = {
    debug("Completing " + item)
    unpackedMap += (item.packed, item.realization.seq) -> item.dual
    delegate.complete(item.dual, continue)
  }
}