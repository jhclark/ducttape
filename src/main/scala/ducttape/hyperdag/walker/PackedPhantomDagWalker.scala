// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag.walker

import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.PhantomHyperDag
import ducttape.hyperdag.meta.PhantomMetaHyperDag

// iterates over non-None packed vertices
class PackedPhantomDagWalker[V](dag: PhantomHyperDag[V,_,_]) extends Walker[PackedVertex[V]] {
  
  val delegate = new PackedDagWalker[Option[V]](dag.delegate)
  
  override def take(): Option[PackedVertex[V]] = delegate.take() match {
    case None => None
    case Some(v) if (dag.isPhantom(v)) => None
    case Some(v) => Some(dag.removePhantom(v))
  }
  
  override def complete(item: PackedVertex[V], continue: Boolean = true) = {
    delegate.complete(dag.toOption(item), continue)
  }
  
  def getCompleted(): Traversable[PackedVertex[V]] = dag.removePhantoms(delegate.getCompleted)
  def getRunning(): Traversable[PackedVertex[V]] = dag.removePhantoms(delegate.getRunning)
  def getReady(): Traversable[PackedVertex[V]] = dag.removePhantoms(delegate.getReady)
}