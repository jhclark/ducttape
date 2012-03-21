package ducttape.syntax 

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

import java.io.File

object BashParser extends App with RegexParsers {
  	
  println("hi")

  override val skipWhitespace = false
  // use file over reader to provide more informative error messages
  // file must be UTF-8 encoded

  val goodResult: ParseResult[String] = parseAll(BashGrammar.bashBlock, """
#!/usr/bin/env bash
x=$y
echo 'hi'
function ohai {
  $0
}
"""
)
  println(goodResult)

  val badResult: ParseResult[String] = parseAll(BashGrammar.bashBlock, """
#!/usr/bin/env bash
x=$y
echo 'hi'
function ohai {
  $0
"""
)
  println(badResult)

}

/**
 * Very simple grammar for bash
 * 
 * @author Jon Clark
 */
object BashGrammar {

  import BashParser._

  // === WHITE SPACE ===

  /** End of line characters, including end of file. */
  val eol: Parser[String] = literal("\r\n") | literal("\n") | regex("""\z""".r) | literal(CharArrayReader.EofCh.toString) 
  /** Non-end of line white space characters */
  val space: Parser[String] = regex("""[ \t]+""".r)
  
  /**
   * End of line, optionally followed by more whitespace, followed by another end of line. 
   * <p>
   * This second end of line is recognized, but not consumed.
   */
  val emptyLine = (literal("\r\n") | literal("\n")) ~ regex("""[ \t]*""".r) ~ guard(eol)
  
  /** Sequence of empty lines. */
  val emptyLines = emptyLine*
    
  /** Non-white space sequence. */
  val nonSpace: Parser[String] = regex("""[^\r\n \t]+""".r)
     
  // === COMMENTS ===

  /** A single line of comment. */
  val comment: Parser[String] = 
    """[ \n\r\t]*#[ \t]*""".r ~> commentContent <~ guard(eol) //| failure("Expected a comment line, but didn't find one.")

  /** The content portion of a single line of comment. 
   *  Notably, this excludes the syntactic comment marker itself. 
   */
  val commentContent: Parser[String] = """[^\r\n]*""".r

  // === NAMES & VALUES ===

  /** Name of a variable, possibly followed by an equals sign.
   *  <p>
   *  The name must conform to Bash variable name requirements: 
   *  "A word consisting solely of letters, numbers, and underscores, and beginning with a letter or underscore."
   *  We make an exception for parameter names, which may be prefixed with a dot, to indicate that they are
   *  Resource Parameters, for use with submitters rather than by the task's bash code
   */
  def bashBlock: Parser[String] = {
    regex("""[^{}]*""".r) ~ opt( literal("{") ~ bashBlock ~ literal("}") ~ regex("""[^{}]*""".r)) ^^ {
      case before ~ None => before
      case before ~ Some(open ~ b ~ close ~ after) => before + "{" + b + "}" + after
    }
  }
}
