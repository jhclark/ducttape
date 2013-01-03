package ducttape.hyperdag.meta
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.UnpackedVertex

/**
 * All incoming metaedges to a vertex will be assigned exactly one of their member hyperedges.
 * This is represented by the sequence "edges". The parentRealizations are actually parallel
 * with the number of incoming metaedges for a vertex. The next sequence is parallel with the
 * number of edges in the active hyperedge for our current path (the one in this unpacking).
 * TODO: Document an example of how to iterate over this easily with zip()
 *
 * Like UnpackedVertex, this interface explicitly avoids giving unpacked vertices as
 * parents so that we can eventually discard more of the explored space
 * the prevState can store information such as "what realizations does my parent have?"
 * NOTE: It's actually the incoming edges that are meta -- not the vertex itself
 * 
 * parentRealizations is parallel (i.e. has the same array length) with "edges", which is
 *   the hyperedges associated with this metaedge
 *   the second layer of sequences is parallel with the plain edges inside each hyperedge (Seq[Seq[D]]).
 *   the inner-most sequence represents a realization (Seq[D]).
 *
 * For readability, you could mentally typedef:
 * type Realization = Seq[D]
 * type HyperEdgeParentRealizations = Seq[Realization]
 * type MetaEdgeParentRealizations = Seq[Seq[Realization]]
 *
 * dual: The "dualistic" representation of this meta vertex in underlying HyperDag (not MetaHyperDag)
 *       used to represent this MetaHyperDag.
 *
 * see [[ducttape.hyperdag.meta.MetaHyperDag]] for definitions of generic types
 */
class UnpackedMetaVertex[V,H,E,D](val packed: PackedVertex[V],
                                  val edges: Seq[HyperEdge[H,E]],
                                  val realization: Seq[D],
                                  val parentRealizations: Seq[Seq[Seq[D]]],
                                  private[hyperdag] val dual: UnpackedVertex[V,H,E,D]) {
  // TODO: More smearing of hash codes
  override def hashCode() = packed.id ^ realization.hashCode
  override def equals(that: Any) = that match {
    case other: UnpackedMetaVertex[_,_,_,_] => (other.packed.id == this.packed.id) && (other.realization == this.realization)
  }
  override def toString() = "%s/%s".format(packed, realization.mkString("-"))
}
