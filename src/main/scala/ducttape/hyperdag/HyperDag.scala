package ducttape.hyperdag

import collection._
import ducttape.viz._
import ducttape.hyperdag.walker.ConstraintFilter
import ducttape.hyperdag.walker.ComboTransformer
import ducttape.hyperdag.walker.VertexFilter
import ducttape.hyperdag.walker.SelectionFilter
import ducttape.hyperdag.walker.HyperEdgeFilter
import ducttape.hyperdag.walker.DefaultConstraintFilter
import ducttape.hyperdag.walker.DefaultComboTransformer
import ducttape.hyperdag.walker.DefaultVertexFilter
import ducttape.hyperdag.walker.DefaultSelectionFilter
import ducttape.hyperdag.walker.DefaultHyperEdgeFilter
import ducttape.hyperdag.walker.DefaultToD

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
  def unpackedWalker[D,F](selectionFilter: SelectionFilter[D] = new DefaultSelectionFilter[D],
                          hedgeFilter: HyperEdgeFilter[H,E] = new DefaultHyperEdgeFilter[H,E],
                          constraintFilter: ConstraintFilter[V,D,F] = new DefaultConstraintFilter[V,D,F],
                          vertexFilter: VertexFilter[V,H,E,D] = new DefaultVertexFilter[V,H,E,D],
                          comboTransformer: ComboTransformer[H,E,D] = new DefaultComboTransformer[H,E,D],
                          toD: H => D = new DefaultToD[H])
    = new walker.UnpackedDagWalker[V,H,E,D,F](this, selectionFilter, hedgeFilter, constraintFilter, vertexFilter,
                                              comboTransformer, toD)
    
  def inEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = inEdgesMap.getOrElse(v, Seq.empty)
  def outEdges(v: PackedVertex[_]): Seq[HyperEdge[H,E]]
    = outEdgesMap.getOrElse(v, Seq.empty)
  def parents(v: PackedVertex[_]): Seq[PackedVertex[V]]
    = for (e <- inEdges(v); src <- sources(e)) yield src
  def children(v: PackedVertex[_]): Seq[PackedVertex[V]]
    = for (e <- outEdges(v)) yield sink(e)
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
    for (v: PackedVertex[V] <- vertexList) {
      for (ant: PackedVertex[V] <- parentsFunc(v)) {
        str ++= "\"%s\" -> \"%s\"\n".format(GraphViz.escape(stringify(ant)), GraphViz.escape(stringify(v)))
      }
    }
    str ++= "}\n"
    str.toString
  }
}
