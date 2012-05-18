package ducttape.hyperdag.walker

import ducttape.hyperdag._
import ducttape.hyperdag.meta.MetaHyperDag

// our only job is to hide epsilon vertices during iteration
// TODO: Create trait for getCompleted etc
class PackedMetaDagWalker[V](val dag: MetaHyperDag[V,_,_,_])
  extends Walker[PackedVertex[V]] {
  
  val delegateWalker = new PackedDagWalker[V](dag.delegate)
  
  // TODO: Filter epsilons and phantoms from completed
  def getCompleted(): Traversable[PackedVertex[V]] = delegateWalker.getCompleted
  def getRunning(): Traversable[PackedVertex[V]] = delegateWalker.getRunning
  def getReady(): Traversable[PackedVertex[V]] = delegateWalker.getReady

  override def complete(item: PackedVertex[V], continue: Boolean = true) = delegateWalker.complete(item, continue)

  // never return epsilon vertices nor phantom vertices
  // we're guaranteed to only have one epsilon vertex in between vertices (no chains)
  // but phantom vertices break this
  override def take(): Option[PackedVertex[V]] = delegateWalker.take() match {
    case Some(result) if (dag.shouldSkip(result)) => {
      complete(result)
      take()
    }
    case opt => opt
  }
}