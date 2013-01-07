package ducttape.util

// TODO: Implicit variant?
object Strings {
  def splitOn(str: String, literal: String): Seq[String] = {
    str.split(str, literal, String.LITERAL).toSeq
  }
}
