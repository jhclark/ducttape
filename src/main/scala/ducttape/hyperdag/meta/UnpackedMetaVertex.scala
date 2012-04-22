package ducttape.hyperdag.meta
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.UnpackedVertex

/**
 *  Unlike a plain UnpackedVertex, with a MetaHyperDAG, our choice of incoming
 * Meta edge may affect what our previous constraint state is (since we traverse
 * multiple hyperedges). Therefore, we must include a mapping from metaEdge to prevState
 *
 * All incoming metaedges to a vertex will be assigned exactly one of their member hyperedges.
 * This is represented by the sequence "edges". The parentRealizations are actually parallel
 * with the number of incoming metaedges for a vertex. The next sequence is parallel with the
 * number of edges in the active hyperedge for our current path (the one in this unpacking).
 * TODO: Document an example of how to iterate over this easily with zip()
 *
 * Like UnpackedVertex, this interface explicitly avoids giving unpacked vertices as
 * parents so that we can eventually discard more of the explored space
 * the prevState can store information such as "what realizations does my parent have?"
 * NOTE: It's actually the incoming edges that are meta -- not the vertex itself */
class UnpackedMetaVertex[V,H,E,D](val packed: PackedVertex[V],
                                  val edges: Seq[HyperEdge[H,E]],
                                  val realization: Seq[D],
                                  val parentRealizations: Seq[Seq[Seq[D]]],
                                  private[hyperdag] val dual: UnpackedVertex[V,H,E,D]) {
  // TODO: More smearing of hash codes
  override def hashCode = packed.id ^ realization.hashCode
  override def equals(that: Any) = that match {
    case other: UnpackedMetaVertex[_,_,_,_] => (other.packed.id == this.packed.id) && (other.realization == this.realization)
  }
  override def toString = packed.toString + " (realization=" + realization.toString +")"
}