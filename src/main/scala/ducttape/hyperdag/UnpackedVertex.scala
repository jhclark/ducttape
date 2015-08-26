// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag

/**
 * An UnpackedVertex is returned by a walker while traversing and
 * unpacking a HyperDAG.
 * 
 * Note: this interface explicitly avoids giving unpacked vertices as
 * parents so that we cabn eventually discard more of the explored space
 * the prevState can store information such as "what realizations does my parent have?"
 *
 * @param parentRealizations The realizations for each parent of this vertex.
 *                           The ordering of the parent realizations is parallel with the 
 *                           ordering returned by [[HyperDag.parents()]]
 *
 * see [[ducttape.hyperdag.HyperDag]] for definitions of generic types V, H, E
 * see [[ducttape.hyperdag.walker.UnpackedDagWalker]] for definition of generic type D
 */
class UnpackedVertex[V,H,E,D](val packed: PackedVertex[V],
                              val edge: Option[HyperEdge[H,E]],
                              val realization: Seq[D],
                              val parentRealizations: Seq[Seq[D]]) {
  // TODO: More smearing of hash codes
  override def hashCode() = packed.id ^ realization.hashCode
  override def equals(that: Any) = that match {
    case other: UnpackedVertex[_,_,_,_] => (other.packed.id == this.packed.id) && (other.realization == this.realization)
  }
  override def toString() = s"${packed}/${realization.mkString("+")}"
}
