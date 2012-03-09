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
  
  /** End of line characters, including end of file. */
  val eol: Parser[String] = literal("\r\n") | literal("\n") | regex("""\z""".r) | literal(CharArrayReader.EofCh.toString) 
  
  /** Non-end of line white space characters */
  val space: Parser[String] = regex("""[ \t]+""".r)
  
  /**
   * Parser for a literal value.
   * <p>
   * The literal value may be quoted or unquoted.
   * 
   * @see quotedLiteral
   * @see unquotedLiteral
   */
  val literalValue : Parser[String] = {
    unquotedLiteral | quotedLiteral
  }
  
  /**
   * Parser for a literal value that is not wrapped in quotes.
   * <p>
   * An unquoted literal is defined as a string 
   * whose first character is neither whitespace nor a (double or single) quotation mark.
   * 
   * If the unquoted literal is more than one character long,
   * any subsequent characters may be any character except whitespace.
   */
  val unquotedLiteral : Parser[String] = {
    regex("""[^"'\s]\S*""".r) | failure("An unquoted literal may not begin with whitespace or a quotation mark")
  }

  /**
   * Parser for a literal value that is wrapped in quotes.
   * <p>
   * An quoted literal is defined as a string 
   * whose first character is a quotation mark
   * and whose last character is an unescaped quotation mark.
   * 
   * Either single (') or double (") quotation marks may be used,
   * but the opening and closing quotation marks must match.
   * <p>
   * If there are any characters between the opening and closing quotation marks,
   * these characters may be any character except the type of quotation mark being used.
   *  
   * Note that the last character between the quotation marks 
   * may not be an unescaped slash (\), 
   * as this would cause the final quotation mark to be escaped.
   * <p>
   * The quoted text may contain escaped sequences.
   * In the string returned by the parser, any such escaped sequences will be expanded.
   */
  val quotedLiteral : Parser[String] = {
    ( regex(""""([^\\"]|\\.)*"""".r) | 
      regex("""'([^\\']|\\.)*'""".r) 
    ) ^^ {
      case string:String => 
         // Remove initial and final quotation marks
         string.substring(1,string.length()-1)
         //     expand escaped form feed characters
               .replace("""\f""","\f")
         //     expand escaped newline characters      
               .replace("""\n""","\n")
         //     expand escaped carriage return characters               
               .replace("""\r""","\r")
         //     expand escaped tab characters                
               .replace("""\t""","\t")
         //     expand escaped slash characters               
               .replace("""\\""","\\")               
    }  
  }
  
  
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
