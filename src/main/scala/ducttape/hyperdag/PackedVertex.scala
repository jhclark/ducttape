package ducttape.hyperdag

class PackedVertex[V](private[hyperdag] val id: Int, val value: V) {
  override def hashCode = id
  override def equals(that: Any) = that match { case other: PackedVertex[_] => (other.id == this.id) }
  override def toString = if(value == null) "ID=%d".format(id) else value.toString
}