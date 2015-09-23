// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag.meta
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.HyperEdge

/**
 * In a MetaHyperDag, all vertices have only MetaEdges as their only incoming "edges".
 * In drawn notation, meta edges are typically drawn as thick arrows with thin
 * incoming hyperdges. In GraphViz, we represent a MetaEdge as an "epsilon" vertex
 * in the AND-OR representation of a MetaHyperDag.
 * 
 * A derivation (beginning from some vertex) consists of each input MetaEdge having
 * exactly one input HyperEdge assigned to it. Unlike a HyperDag, in which
 * only one HyperEdge per vertex can be active in a single derivation,
 * a MetaHyperDag requires that *all* input MetaEdges be active for each
 * vertex.
 *
 * see [[ducttape.hyperdag.meta.MetaHyperDag]] for definitions of generic types
 */
class MetaEdge[M,H,E](private[hyperdag] val epsilonV: PackedVertex[_],
                      val m: M,
                      val hyperedges: Seq[HyperEdge[H,E]])  {
  override def hashCode() = epsilonV.id
  override def equals(that: Any) = that match {
    case other: MetaEdge[_,_,_] => (other.epsilonV.id == this.epsilonV.id)
  }
  override def toString() = m.toString + " " + hyperedges.toString
}
