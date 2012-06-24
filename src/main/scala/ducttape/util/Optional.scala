package ducttape.util

/**
 * Convenience object for wrapping objects in an Option.
 */
object Optional {

  /**
   * Returns <code>None</code> if <code>a</code> is null,
   * <code>Some(a)</code> otherwise.
   * 
   * @param a A (possibly null) object
   */
  def toOption[A](a: A): Option[A] = if(a == null) None else Some(a)
  
}