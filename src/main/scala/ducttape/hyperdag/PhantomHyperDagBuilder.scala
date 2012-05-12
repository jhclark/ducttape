package ducttape.hyperdag

class PhantomHyperDagBuilder[V,H,E] {

  val delegate = new HyperDagBuilder[Option[V],H,E]

  def addVertex(v: V): PackedVertex[Option[V]] = delegate.addVertex(Some(v))
  
  def addPhantomVertex(comment: Option[String] = None): PackedVertex[Option[V]]
    = delegate.addVertex(None, comment)
  
  def addHyperEdge(h: H, sourcePairs: Seq[(PackedVertex[Option[V]],E)], sink: PackedVertex[Option[V]]): HyperEdge[H,E]
    = delegate.addHyperEdge(h, sourcePairs, sink)
  
  def build(): PhantomHyperDag[V,H,E] = {
    new PhantomHyperDag(delegate.build())
  }
}