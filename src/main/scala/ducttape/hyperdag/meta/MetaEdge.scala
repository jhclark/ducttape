package ducttape.hyperdag.meta
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.HyperEdge

// m is the payload of this metahyperedge
// In a MetaHyperDag, all vertices have only MetaEdges as their only inputs.
// A derivation rooted at a vertex consists of each input MetaEdges having
// exactly one input HyperEdge assigned to it. Unlike a HyperDag, in which
// only one HyperEdge per vertex can be active in a single derivation,
// a MetaHyperDag requires that *all* input MetaEdges be active for each
// vertex.
class MetaEdge[M,H,E](private[hyperdag] val epsilonV: PackedVertex[_],
                      val m: M,
                      val hyperedges: Seq[HyperEdge[H,E]])  {
  override def hashCode = epsilonV.id
  override def equals(that: Any) = that match {
    case other: MetaEdge[_,_,_] => (other.epsilonV.id == this.epsilonV.id)
  }
  override def toString = m.toString + " " + hyperedges.toString
}