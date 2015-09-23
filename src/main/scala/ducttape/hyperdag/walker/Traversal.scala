// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag.walker

import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.UnpackedVertex

import java.util.Comparator
import collection._

trait Traversal

case object Arbitrary extends Traversal {
  def comparator[V,H,E,D]() = new Comparator[UnpackedVertex[V,H,E,D]]() {
    // Always return zero
    def compare(o1: UnpackedVertex[V,H,E,D], o2: UnpackedVertex[V,H,E,D]) = 0
  }
  override def toString() = "arbitrary"
}

case object BreadthFirst extends Traversal {
  def comparator[V,H,E,D](vertexIDs: Map[(PackedVertex[V],Seq[D]),Int]) = new Comparator[UnpackedVertex[V,H,E,D]]() {
    def compare(o1: UnpackedVertex[V,H,E,D], o2: UnpackedVertex[V,H,E,D]): Int = {
      // some vertices may not be found due to the vertexFilter -- give these priority -1
      val id1 = vertexIDs.getOrElse( (o1.packed,o1.realization), -1 )
      val id2 = vertexIDs.getOrElse( (o2.packed,o2.realization), -1 )
      id1.compareTo(id2)
    }
  }
  override def toString() = "breadth-first"
}


case object DepthFirst extends Traversal {
  def comparator[V,H,E,D](vertexIDs: Map[(PackedVertex[V],Seq[D]),Int]) = new Comparator[UnpackedVertex[V,H,E,D]]() {
    def compare(o1: UnpackedVertex[V,H,E,D], o2: UnpackedVertex[V,H,E,D]): Int = {
      // some vertices may not be found due to the vertexFilter -- give these priority -1
      val id1 = vertexIDs.getOrElse( (o1.packed,o1.realization), -1 )
      val id2 = vertexIDs.getOrElse( (o2.packed,o2.realization), -1 )
      id2.compareTo(id1)
    }
  }
  override def toString() = "depth-first"
}

