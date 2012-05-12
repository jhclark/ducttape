package ducttape.hyperdag

class PhantomHyperDag[V,H,E](val delegate: HyperDag[Option[V],H,E]) {
  lazy val vertices = delegate.vertices.filter { _.value.isDefined } // TODO: wrapper removing option
  lazy val size = vertices.size
  
  private[hyperdag] def removePhantom(v: PackedVertex[Option[V]]): PackedVertex[V] = v.value match {
    case Some(value) => new PackedVertex[V](v.id, value, v.comment)
  }
  
  private[hyperdag] def toOption(v: PackedVertex[V]): PackedVertex[Option[V]]
    = new PackedVertex[Option[V]](v.id, Some(v.value), v.comment)
  
  private[hyperdag] def removePhantoms(list: Traversable[PackedVertex[Option[V]]]) = list.filter(_.value.isDefined).map {
    v => v.value match {
      case Some(value) => new PackedVertex[V](v.id, value)
    }
  }
  
  def isPhantom(v: PackedVertex[Option[V]]) = v.value.isEmpty
  
  def inEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = delegate.inEdges(v)
  def outEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = delegate.outEdges(v)
  def parents(v: PackedVertex[_]): Seq[PackedVertex[V]]
    = for (e <- inEdges(v); src <- sources(e)) yield src
  def children(v: PackedVertex[_]): Seq[PackedVertex[V]]
    = for (e <- outEdges(v)) yield sink(e)
  def sources(e: HyperEdge[H,E]): Seq[PackedVertex[V]]
    = removePhantoms(delegate.sources(e)).toSeq
  def sink(e: HyperEdge[H,E]): PackedVertex[V]
    = removePhantom(delegate.sink(e))
    
  def toGraphViz(vertexList: Seq[PackedVertex[Option[V]]],
                 inEdgesFunc: PackedVertex[Option[V]] => Seq[HyperEdge[H,E]],
                 stringify: PackedVertex[Option[V]] => String): String
    = delegate.toGraphViz(vertexList, inEdgesFunc, stringify)

  def toGraphViz(): String = toGraphViz() 

  
  // TODO: Add a lightweight wrapper over a packed vertex with an option
  // that exposes value as a get operation -- only return these for non-phantom vertices
  // in the unpacked walker
}
