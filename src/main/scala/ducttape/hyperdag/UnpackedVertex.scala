package ducttape.hyperdag

/** this interface explicitly avoids giving unpacked vertices as
 * parents so that we can eventually discard more of the explored space
 * the prevState can store information such as "what realizations does my parent have?" */
class UnpackedVertex[V,H,E,D](val packed: PackedVertex[V],
                              val edge: Option[HyperEdge[H,E]],
                              val realization: Seq[D],
                              val parentRealizations: Seq[Seq[D]]) {
  // TODO: More smearing of hash codes
  override def hashCode() = packed.id ^ realization.hashCode
  override def equals(that: Any) = that match {
    case other: UnpackedVertex[_,_,_,_] => (other.packed.id == this.packed.id) && (other.realization == this.realization)
  }
  override def toString() = packed.toString + " (realization=" + realization.toString +")"
}