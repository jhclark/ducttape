package ducttape.hyperdag.meta
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.UnpackedVertex

/** Represents a chain of phantom vertices in an unpacked PhantomMetaHyperDAG */
class UnpackedChainedMetaVertex[V,H,E,D](
    val packed: PackedVertex[Option[V]],
    val edges: Seq[Seq[E]], // TODO: XXX: We got rid of HyperEdges instead of adding a list of them to keep things simple
    val realization: Seq[D],
    val parentRealizations: Seq[Seq[Seq[D]]],
    private[hyperdag] val dual: UnpackedMetaVertex[Option[V],H,E,D]) {
  
  // TODO: More smearing of hash codes
  override def hashCode() = packed.id ^ realization.hashCode
  override def equals(that: Any) = that match {
    case other: UnpackedChainedMetaVertex[_,_,_,_] => (other.packed.id == this.packed.id) && (other.realization == this.realization)
  }
  override def toString() = packed.toString + " (realization=" + realization.toString +")"
}