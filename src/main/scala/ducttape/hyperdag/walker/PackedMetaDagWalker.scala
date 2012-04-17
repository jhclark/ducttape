package ducttape.hyperdag.walker

import ducttape.hyperdag._
import ducttape.hyperdag.meta.MetaHyperDag

// our only job is to hide epsilon vertices during iteration
// TODO: Create trait for getCompleted etc
class PackedMetaDagWalker[V](val dag: MetaHyperDag[V,_,_,_])
  extends Walker[PackedVertex[V]] {
  
  val delegate = new PackedDagWalker[V](dag.delegate)

  // TODO: Filter epsilons and phantoms from completed
  def getCompleted(): Traversable[PackedVertex[V]] = delegate.getCompleted
  def getRunning(): Traversable[PackedVertex[V]] = delegate.getRunning
  def getReady(): Traversable[PackedVertex[V]] = delegate.getReady

  override def complete(item: PackedVertex[V], continue: Boolean = true) = delegate.complete(item, continue)

  override def take(): Option[PackedVertex[V]] = {
    var result = delegate.take()
    // never return epsilon vertices nor phantom vertices
    // we're guaranteed to only have one epsilon vertex in between vertices (no chains)
    // but phantom vertices break this
    while (!result.isEmpty && (dag.shouldSkip(result.get))) {
      complete(result.get)
      result = delegate.take()
    }
    return result
  }
}