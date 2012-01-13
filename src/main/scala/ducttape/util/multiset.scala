package ducttape.util

import collection._

object MultiSet {
  // TODO: Can we make this more efficient
  def empty[A] = new MultiSet[A]
}

class MultiSet[A] {
  private val map = new mutable.HashMap[A,Int]

  def +=(a: A) {
    map.get(a) match {
      case Some(n) => map.put(a, n+1)
      case None => map.put(a, 1)
    }
  }

  def -=(a: A) {
    map.get(a) match {
      case Some(n) if n == 1 => map.remove(a)
      case Some(n) if n > 1 => map.put(a, n-1)
      case None => throw new NoSuchElementException(a.toString)
    }
  }

  def ++=(xs: TraversableOnce[A]) = for(x <- xs) this += x
  def --=(xs: TraversableOnce[A]) = for(x <- xs) this -= x
  def apply(a: A) = map.contains(a)
  def toList(): List[A] = map.keys.toList
  override def toString() = map.toString
}
