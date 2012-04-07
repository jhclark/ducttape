package ducttape.hyperdag

import collection._

import ducttape.viz._

// immutable
class HyperDag[V,H,E](val roots: Seq[PackedVertex[V]],
                      val vertices: Seq[PackedVertex[V]],
                      private[hyperdag] val inEdgesMap: Map[PackedVertex[_], Seq[HyperEdge[H,E]]],
                      private[hyperdag] val outEdgesMap: Map[PackedVertex[_], Seq[HyperEdge[H,E]]],
                      private[hyperdag] val edges: Map[HyperEdge[H,E], (Seq[PackedVertex[V]],PackedVertex[V])]) {
                       
  val size: Int = vertices.size

  def packedWalker()
    = new walker.PackedDagWalker[V](this)
  // TODO: Pass filters to dag walker
  def unpackedWalker()
    = new walker.UnpackedDagWalker[V,H,E,Null](this)
  def inEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = inEdgesMap.getOrElse(v, Seq.empty)
  def outEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = outEdgesMap.getOrElse(v, Seq.empty)
  def parents(v: PackedVertex[_]): Seq[PackedVertex[V]]
    = for(e <- inEdges(v); src <- sources(e)) yield src
  def children(v: PackedVertex[_]): Seq[PackedVertex[V]]
    = for(e <- outEdges(v)) yield sink(e)
  def sources(e: HyperEdge[H,E]): Seq[PackedVertex[V]]
    = edges(e)._1
  def sink(e: HyperEdge[H,E]): PackedVertex[V]
    = edges(e)._2

  def toGraphViz(): String = toGraphViz(vertices, parents, {v => v.toString}) 

  def toGraphViz(vertexList: Seq[PackedVertex[V]],
                 parentsFunc: PackedVertex[V] => Seq[PackedVertex[V]],
                 stringify: PackedVertex[V] => String): String = {
    val str = new StringBuilder(1000)
    str ++= "digraph G {\n"
    for(v: PackedVertex[V] <- vertexList) {
      for(ant: PackedVertex[V] <- parentsFunc(v)) {
        str ++= "\"%s\" -> \"%s\"\n".format(GraphViz.escape(stringify(ant)), GraphViz.escape(stringify(v)))
      }
    }
    str ++= "}\n"
    str.toString
  }
}
