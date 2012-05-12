package ducttape.hyperdag.walker
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.util.MultiSet
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.UnpackedVertex
import ducttape.hyperdag.meta.UnpackedChainedMetaVertex

// these really better belong in companion objects
// for UnpackedDagWalker and UnpackedMetaDagWalker
// however, that caused inscrutable compiler errors
// for some bizarre reasons

trait MetaVertexFilter[V,H,E,D] {
  def apply(v: UnpackedMetaVertex[V,H,E,D]): Boolean
}

class DefaultMetaVertexFilter[V,H,E,D] extends MetaVertexFilter[V,H,E,D] {
  override def apply(v: UnpackedMetaVertex[V,H,E,D]) = true
}

trait SelectionFilter[H] {
  def apply(combo: MultiSet[H]): Boolean
}

class DefaultSelectionFilter[H] extends SelectionFilter[H] {
  override def apply(combo: MultiSet[H]) = true
}

trait HyperEdgeFilter[H,E] {
  def apply(he: HyperEdge[H,E]): Boolean
}

class DefaultHyperEdgeFilter[H,E] extends HyperEdgeFilter[H,E] {
  override def apply(combo: HyperEdge[H,E]) = true
}

trait ConstraintFilter[V,D,F] {
  def apply(v: PackedVertex[V], prevState: F, combo: MultiSet[D], parentRealization: Seq[D]): Option[F]
  val initState: F
}

class DefaultConstraintFilter[V,D,F] extends ConstraintFilter[V,D,F] {
  override def apply(v: PackedVertex[V], prevState: F, combo: MultiSet[D], parentRealization: Seq[D]) = Some(prevState)
  private var nada: F = _ // syntactic cruft, just to get a null...
  override val initState: F = nada
}

trait VertexFilter[V,H,E,D] {
  def apply(v: UnpackedVertex[V,H,E,D]): Boolean
}

class DefaultVertexFilter[V,H,E,D] extends VertexFilter[V,H,E,D] {
  override def apply(v: UnpackedVertex[V,H,E,D]) = true
}

class DefaultToD[H] extends Function1[H,H] {
  override def apply(h: H) = h
}

// TODO: Receieve immutable multiset as argument?
trait ComboTransformer[H,E,D] {
  def apply(he: Option[HyperEdge[H,E]], combo: MultiSet[D]): Option[MultiSet[D]]
}

class DefaultComboTransformer[H,E,D] extends ComboTransformer[H,E,D] {
  override def apply(he: Option[HyperEdge[H,E]], combo: MultiSet[D]) = Some(combo)
}