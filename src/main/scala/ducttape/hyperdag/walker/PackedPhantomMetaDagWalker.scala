// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag.walker

import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.PhantomHyperDag
import ducttape.hyperdag.meta.PhantomMetaHyperDag

// iterates over non-None packed vertices
class PackedPhantomMetaDagWalker[V](dag: PhantomMetaHyperDag[V,_,_,_])
    extends Walker[PackedVertex[Option[V]]] {
  
  val delegate = new PackedMetaDagWalker[Option[V]](dag.delegate)
  
  override def take(): Option[PackedVertex[Option[V]]] = delegate.take() match {
    case None => None
    case candidate @ Some(v) => v.value match {
      case None => {
        delegate.complete(v)
        take()
      }
      case Some(_) => candidate
    }
  }
  
  override def complete(item: PackedVertex[Option[V]], continue: Boolean = true) = {
    delegate.complete(item, continue)
  }
  
  def getCompleted(): Traversable[PackedVertex[Option[V]]] = dag.removePhantoms(delegate.getCompleted)
  def getRunning(): Traversable[PackedVertex[Option[V]]] = dag.removePhantoms(delegate.getRunning)
  def getReady(): Traversable[PackedVertex[Option[V]]] = dag.removePhantoms(delegate.getReady)
}