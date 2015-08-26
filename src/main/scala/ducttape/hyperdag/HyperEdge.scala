// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag

import ducttape.util.Optional

/**
 * A hyperedge is composed of one or more "incoming" edges
 * and has a single implicit "outgoing" edge.
 *
 * e will be isomorophic to the sequence of source vertices
 * returned by HyperDag.sources(hyperdge)
 *
 * see [[ducttape.hyperdag.HyperDag]] for definitions of generic types
 */
class HyperEdge[H,E](private[hyperdag] val id: Int, val h: H, val e: Seq[E]) {
  override def hashCode() = id
  override def equals(that: Any) = that match { case other: HyperEdge[_,_] => (other.id == this.id) }
  def toString(withNewlines: Boolean) = {
    val hStr = Optional.toOption(h) match {
      case None => "ID=%d".format(id)
      case Some(_) => h.toString + ":" + id
    }
    val eStr = e.filter { _ != null } match {
      case Seq() => ""
      case _ => ":" + e.mkString(if (withNewlines) "\n" else " ")
    }
    hStr + eStr
  }
  override def toString() = toString(withNewlines=false)
}
