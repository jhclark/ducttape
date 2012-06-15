package ducttape.hyperdag.walker

import ducttape.hyperdag.PhantomHyperDag
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.UnpackedVertex
import ducttape.util.MultiSet
import ducttape.hyperdag.PackedVertex
import grizzled.slf4j.Logging

// TODO: Create UnpackedPhantomVertex and return those instead
class UnpackedPhantomDagWalker[V,H,E,D,F](
        val dag: PhantomHyperDag[V,H,E],
        val selectionFilter: SelectionFilter[D] = new DefaultSelectionFilter[D],
        val hedgeFilter: HyperEdgeFilter[H,E] = new DefaultHyperEdgeFilter[H,E],
        val constraintFilter: ConstraintFilter[V,H,E,D,F] = new DefaultConstraintFilter[V,H,E,D,F],
        val vertexFilter: VertexFilter[V,H,E,D] = new DefaultVertexFilter[V,H,E,D],
        val comboTransformer: ComboTransformer[H,E,D] = new DefaultComboTransformer[H,E,D],
        val toD: H => D = new DefaultToD[H])
       (implicit ordering: Ordering[D])
  extends Walker[UnpackedVertex[V,H,E,D]] with Logging {
  
  object ConstraintFilterAdapter extends ConstraintFilter[Option[V],H,E,D,F] {
    override val initState = constraintFilter.initState
    override def apply(v: PackedVertex[Option[V]], he: Option[HyperEdge[H,E]], prevState: F, combo: MultiSet[D], parentRealization: Seq[D]) = {
      v.value match {
        case None => Some(prevState)
        case Some(_) => constraintFilter(dag.removePhantom(v), he, prevState, combo, parentRealization)
      }
    }
  }
 
  object VertexFilterAdapter extends VertexFilter[Option[V],H,E,D] {
    override def apply(v: UnpackedVertex[Option[V],H,E,D]) = v.packed.value match {
      case Some(_) => vertexFilter(removePhantom(v))
      case None => true
    }
  }
  
  private[hyperdag] def removePhantom(v: UnpackedVertex[Option[V],H,E,D])
    = new UnpackedVertex[V,H,E,D](dag.removePhantom(v.packed), v.edge, v.realization, v.parentRealizations)

  private[hyperdag] def toOption(v: UnpackedVertex[V,H,E,D])
    = new UnpackedVertex[Option[V],H,E,D](dag.toOption(v.packed), v.edge, v.realization, v.parentRealizations)
    
  def isPhantom(v: UnpackedVertex[Option[V],H,E,D]) = v.packed.value.isEmpty
    
  val delegate = new UnpackedDagWalker[Option[V],H,E,D,F](
    dag.delegate, selectionFilter, hedgeFilter,  ConstraintFilterAdapter, VertexFilterAdapter, comboTransformer, toD)
    
  override def take(): Option[UnpackedVertex[V,H,E,D]] = delegate.take() match {
    case None => None
    case Some(v) => {
      if (isPhantom(v)) {
        delegate.complete(v)
        debug("Phantom skipping: " + v)
        this.take()
      } else {
        throw new Error("Class Unimplemented")
      }
    }
  }
  
  override def complete(item: UnpackedVertex[V,H,E,D], continue: Boolean = true) = {
    delegate.complete(toOption(item), continue)
  }
}
