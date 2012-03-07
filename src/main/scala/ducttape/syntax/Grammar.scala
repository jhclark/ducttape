package ducttape.syntax

import java.io.File
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional
import ducttape.syntax.GrammarParser._
import ducttape.syntax.AbstractSyntaxTree._
import scala.util.matching.Regex


object Grammar {

  /**
   * Parser for a name, defined as an ASCII alphanumeric identifier.
   * <p>
   * The first character must be an upper-case letter, an lower-case letter, or an underscore.
   * Each subsequent character in the name (if any exist) 
   * must be an upper-case letter, a lower-case letter, a numeric digit, or an underscore.
   * 
   * @param whatCanComeNext Regular expression that specifies what may legally follow the name
   */
  def name(whatCanComeNext:Regex): Parser[String] = {
    ( // If the name starts with an illegal character, bail out and don't backtrack
      """[^A-Za-z_]""".r<~err("Illegal character at start of task name")

      // Else if the name itself is OK, but it is followed by something that can't legally follow the name, bail out and don't backtrack
      | """[A-Za-z_][A-Za-z0-9_]*""".r<~guard(not(regex(whatCanComeNext)))~!err("Illegal character in task name")

      // Finally, if the name contains only legal characters, then parse it!
      | """[A-Za-z_][A-Za-z0-9_]*""".r // | failure("")
    )
  }
  
  /** Name of a task, enclosed in square brackets. 
   *  <p>
   *  The name must conform to Bash variable name requirements: 
   *  "A word consisting solely of letters, numbers, and underscores, and beginning with a letter or underscore."
   */
  def taskName: Parser[String] = {
    "[" ~> name("""\]""".r) <~ "]"
  }
  
  def taskBlock: Parser[TaskDefinition] = positioned(taskName ^^ {
    case string => new TaskDefinition(string)
  })
  
  def tape: Parser[Tape] = positioned(rep(taskBlock) ^^ {
    case sequence => new Tape(sequence)
  })

}
