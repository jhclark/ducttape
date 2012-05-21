package ducttape.hyperdag.meta

import ducttape.hyperdag.PackedVertex

class PhantomMetaHyperDagBuilder[V,M,H,E](epsilonV: V = null, epsilonH: H = null, epsilonE: E = null) {
  
  private val delegate = new MetaHyperDagBuilder[Option[V],M,H,E](Some(epsilonV), epsilonH, epsilonE)
  
  def addPhantomVertex(comment: Option[String] = None): PackedVertex[Option[V]]
    = delegate.addVertex(None, comment)
    
  def addPhantomVertex(comment: String): PackedVertex[Option[V]]
    = delegate.addVertex(None, Some(comment))

  def addVertex(v: V, comment: Option[String] = None): PackedVertex[Option[V]]
    = delegate.addVertex(Some(v), comment)
    
  def addVertex(v: V, comment: String): PackedVertex[Option[V]]
    = delegate.addVertex(Some(v), Some(comment))
  
  def addMetaEdge(m: M,
                  hyperEdgeInfo: Seq[(H, Seq[(PackedVertex[Option[V]],E)])],
                  sink: PackedVertex[Option[V]],
                  comment: Option[String]): MetaEdge[M,H,E] = {
    delegate.addMetaEdge(m, hyperEdgeInfo, sink, comment)
  }
  
  def addMetaEdge(m: M,
                  hyperEdgeInfo: Seq[(H, Seq[(PackedVertex[Option[V]],E)])],
                  sink: PackedVertex[Option[V]],
                  comment: String): MetaEdge[M,H,E] = {
    delegate.addMetaEdge(m, hyperEdgeInfo, sink, Some(comment))
  }
  
  def build() = new PhantomMetaHyperDag[V,M,H,E](delegate.build())
}