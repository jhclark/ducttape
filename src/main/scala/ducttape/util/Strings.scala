package ducttape.util

import collection._
import java.util.regex.Pattern

// TODO: Implicit variant?
object Strings {
  def splitOn(str: String, literal: String): Seq[String] = {
    Pattern.compile(literal, Pattern.LITERAL).split(str).toSeq
  }
}
