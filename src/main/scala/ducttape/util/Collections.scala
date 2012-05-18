package ducttape.util

object Collections {
  def zip3[A,B,C](a: Iterable[A], b: Iterable[B], c: Iterable[C]): Iterable[(A,B,C)] =
    a.zip(b).zip(c).map { case ( (a1, b1), c1) => (a1, b1, c1) }
  
  def zip3[A,B,C](a: Seq[A], b: Iterable[B], c: Iterable[C]): Seq[(A,B,C)] =
    a.zip(b).zip(c).map { case ( (a1, b1), c1) => (a1, b1, c1) }
}