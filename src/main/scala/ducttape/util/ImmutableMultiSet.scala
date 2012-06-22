package ducttape.util

trait ImmutableMultiSet[A] {
  def find(func: A => Boolean): Option[A]
  def removeAll(a: A)
  def apply(a: A)
  def contains(a: A)
  def keys()
  def view()
  def toList(): List[A]
}