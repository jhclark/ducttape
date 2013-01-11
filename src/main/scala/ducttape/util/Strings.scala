package ducttape.util

import collection._
import java.util.regex.Pattern

// TODO: Implicit variant?
object Strings {
  def splitOn(str: String, literal: String): Seq[String] = {
    Pattern.compile(literal, Pattern.LITERAL).split(str).toSeq
  }
  /* returns (prefix, suffix)
   * if delim is not found in str, then str is returned as the suffix
   */
  def splitOnLast(str: String, delim: Char): (Option[String], String) = {
    str.lastIndexOf(delim) match {
      case -1 => (None, str)
      case idx: Int => {
        val before = str.substring(0, idx)
        val after = str.substring(idx+1)
        (Some(before), after)
      }
    }
  }
}
