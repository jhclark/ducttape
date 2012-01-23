package ducttape.util

import scala.collection._

class OrderedSet[A] extends mutable.HashSet[A] {
  private val order = new mutable.ArrayBuffer[A]
  override def addEntry(elem: A) = {
    if(!super.apply(elem)) {
      order += elem
    }
    super.addEntry(elem)
  }
  override def removeEntry(elem: A) = {
    order -= elem
    super.removeEntry(elem)
  }
  override def iterator = order.iterator
  override def foreach[U](f: A => U) = order.foreach(f)
}
