package ducttape.hyperdag

class HyperEdge[H,E](private[hyperdag] val id: Int, val h: H, val e: Seq[E]) {
  override def hashCode = id
  override def equals(that: Any) = that match { case other: HyperEdge[_,_] => (other.id == this.id) }
  override def toString = (if(h == null) "ID=%d;".format(id) else h.toString) + e.toString
}