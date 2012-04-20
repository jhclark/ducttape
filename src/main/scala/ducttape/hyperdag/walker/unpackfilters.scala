package ducttape.hyperdag.walker
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.util.MultiSet
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.UnpackedVertex
import ducttape.hyperdag.AntiHyperEdge

// these really better belong in companion objects
// for UnpackedDagWalker and UnpackedMetaDagWalker
// however, that caused inscrutable compiler errors
// for some bizarre reasons

trait MetaVertexFilter[V,H,E] {
  def apply(v: UnpackedMetaVertex[V,H,E]): Boolean
}

class DefaultMetaVertexFilter[V,H,E] extends MetaVertexFilter[V,H,E] {
  override def apply(v: UnpackedMetaVertex[V,H,E]) = true
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

trait ConstraintFilter[V,H,F] {
  def apply(v: PackedVertex[V], prevState: F, combo: MultiSet[H], parentRealization: Seq[H]): Option[F]
  val initState: F
}

class DefaultConstraintFilter[V,H,F] extends ConstraintFilter[V,H,F] {
  override def apply(v: PackedVertex[V], prevState: F, combo: MultiSet[H], parentRealization: Seq[H]) = Some(prevState)
  private var nada: F = _ // syntactic cruft, just to get a null...
  override val initState: F = nada
}

trait VertexFilter[V,H,E] {
  def apply(v: UnpackedVertex[V,H,E]): Boolean
}

class DefaultVertexFilter[V,H,E] extends VertexFilter[V,H,E] {
  override def apply(v: UnpackedVertex[V,H,E]) = true
}

// TODO: Receieve immutable multiset as argument?
trait ComboTransformer[H,E] {
  def apply(he: Option[HyperEdge[H,E]], combo: MultiSet[H]): Option[MultiSet[H]]
}

class DefaultComboTransformer[H,E] extends ComboTransformer[H,E] {
  override def apply(he: Option[HyperEdge[H,E]], combo: MultiSet[H]) = Some(combo)
}

/** when used with an unpacker, causes anti-hyperedges to be recognized
 *  and handled properly (i.e. required if you want to use AntiHyperEdges) */
class AntiHyperEdgeComboTransformer[H,E] extends ComboTransformer[H,E] {
  override def apply(he: Option[HyperEdge[H,E]], combo: MultiSet[H]) = he match {
    case Some(anti: AntiHyperEdge[_,_]) => {
      if (combo.contains(anti.h)) {
        val copy = new MultiSet[H](combo)
        copy.removeAll(anti.h)
        Some(copy)
      } else {
        // no corresponding edge was found in the derivation
        // this anti-hyperedge cannot apply
        //
        // TODO: Note when corresponding edge not found
        // to help user understand why no path is available
        None
      }
    }
    case _ => Some(combo)
  }
}