package ducttape.hyperdag.meta
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.UnpackedVertex

/** Represents a chain of phantom vertices in an unpacked PhantomMetaHyperDAG.
 *  Because a PhantomMetaHyperDag allows multiple branch points to be introduced
 *  between two "real" (non-phantom, non-epsilon) vertices, we may have encountered multiple
 *  new phantom vertices between these two real vertices. Those phantom vertices form
 *  the "chain" here. In other words, this is a single unpacked meta vertex along with
 *  a chain of phantom vertices that lead to a parent meta vertex. Instead of exposing
 *  all of the intervening structure between the real vertices, we flatten it in this
 *  data structure.
 *
 *  The edges member can be thought of as having the following typedefs:
 *  type FlattenedHyperEdge = Seq[E]
 *  type FlattenedMetaEdge = Seq[FlattenedHyperEdge]
 *
 *  The parentRealizations can be thought of as having the following typedefs:
 *  type Realization = Seq[D]
 *  type FlattenedHyperEdgeParentRealizations = Seq[Realization]
 *  type FlattenedMetaEdgeParentRealizations = Seq[Seq[Realization]]
 *
 *  parentRealizations is parallel (i.e. has the same array length) with "edges", which is
 *    the flattened hyperedges associated with this metaedge
 *    the second layer of sequences is parallel with the plain edges inside each flattened hyperedge (Seq[Seq[D]]).
 *    the inner-most sequence represents a realization (Seq[D]).
 * 
 *  For comparison, see [[ducttape.hyperdag.meta.UnpackedMetaVertex]]
 * 
 *  Note: This class begain its life as having "edges" being a list of HyperEdges, but things
 *        quickly got messy, so we simply flattened them down to a list of edges and completely
 *        hide all phantom and epsilon vertices from the user (via flattening). However, due to
 *        the flattening, we no longer know which phantom vertices these are parallel/isomorphic to.
 *        This means client code does not need recursion to access this data structure.
 *
 * See [[ducttape.hyperdag.meta.MetaHyperDag]] for definitions of generic types.
 */
class UnpackedChainedMetaVertex[V,H,E,D](
    val packed: PackedVertex[Option[V]],
    val edges: Seq[Seq[E]],
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
