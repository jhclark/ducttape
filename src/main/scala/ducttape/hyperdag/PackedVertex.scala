package ducttape.hyperdag

// TODO: Move?
/**
 * A generic, opaque vertex type, which is the parent of all vertices,
 * including phantom and epsilon vertices
 *
 * see [[ducttape.hyperdag.HyperDag]] for definitions of generic types
 * 
 * comment is a comment or label to be displayed in the GraphViz representation of this vertex
 */
class PackedVertex[V](private[hyperdag] val id: Int, val value: V, val comment: Option[String] = None) {
  override def hashCode() = id
  override def equals(that: Any) = that match { case other: PackedVertex[_] => (other.id == this.id) }
  override def toString() = comment match {
    case None => if (value != null) value.toString else  "ID=%d".format(id)
    case Some(str) => "%s:%d".format(str, id)
  }
}
