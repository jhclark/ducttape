package ducttape.util

class MutableOrderedSet[A] extends OrderedSet[A] with scala.collection.mutable.Set[A] {
  protected val order = new scala.collection.mutable.ArrayBuffer[A]
  protected val hash = new scala.collection.mutable.HashSet[A]

  override def +=(elem: A): this.type = {
    if(!hash(elem)) {
      order += elem
      hash += elem
    }
    this
  }

  override def -=(elem: A): this.type = {
    order -= elem
    hash -= elem
    this
  }

  override def +(elem: A): MutableOrderedSet[A] = {
    val result = new MutableOrderedSet[A]
    result.order ++= this.order
    result.hash ++= hash
    result += elem
  }

  override def -(elem: A): MutableOrderedSet[A] = {
    val result = new MutableOrderedSet[A]
    result.order ++= this.order
    result.hash ++= hash
    result -= elem
  }
  
  override def contains(elem: A) = hash.contains(elem)
  override def empty = new MutableOrderedSet[A]
  override def iterator = order.iterator
  override def seq = this
  override def foreach[U](f: A => U) = order.foreach(f)
  override def size = order.size
  
  override def toIterable = order
  override def toIterator = order.iterator
  override def toSeq = order.toSeq
  override def toList = order.toList
  override def toTraversable = order

  override def clear {
    order.clear
    hash.clear
  }
}

/*
object OrderedSet extends MutableSetFactory[OrderedSet] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, OrderedSet[A]] = setCanBuildFrom[A]
  override def empty[A]: OrderedSet[A] = new OrderedSet[A]
}
*/
