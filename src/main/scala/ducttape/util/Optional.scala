package ducttape.util

object Optional {
  def toOption[A](a: A): Option[A] = if(a == null) None else Some(a)
}