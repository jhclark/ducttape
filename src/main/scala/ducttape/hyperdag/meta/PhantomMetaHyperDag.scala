package ducttape.hyperdag.meta

import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.UnpackedVertex
import ducttape.hyperdag.walker.PackedPhantomMetaDagWalker
import ducttape.hyperdag.walker.MetaVertexFilter
import ducttape.hyperdag.walker.DefaultMetaVertexFilter
import ducttape.hyperdag.walker.DefaultToD
import ducttape.hyperdag.walker.UnpackedPhantomMetaDagWalker
import ducttape.hyperdag.walker.RealizationMunger
import ducttape.hyperdag.walker.DefaultRealizationMunger

import collection._

class PhantomMetaHyperDag[V,M,H,E](val delegate: MetaHyperDag[Option[V],M,H,E]) {
  
  def packedWalker() = new PackedPhantomMetaDagWalker[V](this)
  
  def unpackedWalker[D,F](munger: RealizationMunger[Option[V],H,E,D,F],
                          vertexFilter: MetaVertexFilter[Option[V],H,E,D],
                          toD: H => D,
                          observer: UnpackedVertex[Option[V], H,E,D] => Unit)
                         (implicit ordering: Ordering[D])
                         : UnpackedPhantomMetaDagWalker[V,M,H,E,D,F] = {
    new UnpackedPhantomMetaDagWalker[V,M,H,E,D,F](this, munger, vertexFilter, toD, observer)(ordering)
  }
  
  def unpackedWalker[D](vertexFilter: MetaVertexFilter[Option[V],H,E,D] = new DefaultMetaVertexFilter[Option[V],H,E,D],
                        toD: H => D = new DefaultToD[H],
                        observer: UnpackedVertex[Option[V], H,E,D] => Unit = (v: UnpackedVertex[Option[V],H,E,D]) => { ; } )
                       (implicit ordering: Ordering[D]): UnpackedPhantomMetaDagWalker[V,M,H,E,D,immutable.HashSet[D]] = {
    val munger = new DefaultRealizationMunger[Option[V],H,E,D]
    new UnpackedPhantomMetaDagWalker[V,M,H,E,D,immutable.HashSet[D]](this, munger, vertexFilter, toD, observer)(ordering)
  }
  
  private[hyperdag] def removePhantoms(list: Traversable[PackedVertex[Option[V]]])
    = list.filter { isPhantom(_) }
  
  private[hyperdag] def isPhantom(v: PackedVertex[Option[V]]) = v.value.isEmpty

  // TODO: Do we need to strip phantoms from any of these?
  def inMetaEdges(v: PackedVertex[_]): Seq[MetaEdge[M,H,E]]
    = delegate.inMetaEdges(v)
  def inHyperEdges(me: MetaEdge[M,H,E]): Seq[HyperEdge[H,E]]
    = delegate.inHyperEdges(me)
  def outHyperEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = delegate.outHyperEdges(v)
  def outMetaEdge(he: HyperEdge[H,E]): MetaEdge[M,H,E]
    = delegate.outMetaEdge(he)
  // TODO: Edge chain methods?
    
  def vertices() = delegate.vertices.filter(!isPhantom(_))
  
  // TODO: remove phantoms?
  def parents(v: PackedVertex[_]): Seq[PackedVertex[Option[V]]] = delegate.parents(v)
  def children(v: PackedVertex[_]): Seq[PackedVertex[Option[V]]] = delegate.children(v)
  
  // TODO: remove phantoms?
  def sources(e: MetaEdge[M,H,E]): Seq[PackedVertex[Option[V]]] = delegate.sources(e)
  def sources(e: HyperEdge[H,E]): Seq[PackedVertex[Option[V]]] = delegate.sources(e)
  def sink(e: HyperEdge[H,E]): PackedVertex[Option[V]] = sink(outMetaEdge(e))
  def sink(e: MetaEdge[M,H,E]): PackedVertex[Option[V]] = delegate.children(e.epsilonV).head
    
  lazy val size = vertices.size
  
  def toGraphViz(): String = delegate.toGraphViz()
  def toGraphVizDebug(): String = delegate.toGraphVizDebug()
}
