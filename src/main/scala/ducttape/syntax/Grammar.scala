package ducttape.syntax

import java.io.File
import java.math.BigDecimal
import java.math.BigInteger

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional
import scala.util.matching.Regex

import ducttape.syntax.GrammarParser._
import ducttape.syntax.AbstractSyntaxTree._


object Grammar {
  
  /** End of line characters, including end of file. */
  val eol: Parser[String] = literal("\r\n") | literal("\n") | regex("""\z""".r) | literal(CharArrayReader.EofCh.toString) 
  
  /** Non-end of line white space characters */
  val space: Parser[String] = regex("""[ \t]+""".r)
  
  /** A signed, arbitrary precision number. */
  val number: Parser[BigDecimal] = 
    ( // Recognize a number with at least one digit left of the decimal
      // and optionally, one or more digits to the right of the decimal
      regex("""[+-]?\d+(\.\d+)?([eE][-+]?\d+)?""".r)  |
      // Do NOT recognize a number with no digits left of the decimal
      (regex("""[+-]?\.\d+([eE][-+]?\d+)?""".r)~!failure("A number must have at least one digit left of the decimal point.") )
    ) ^^ {
    case s:String => new BigDecimal(s)
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
  val unquotedLiteral : Parser[Literal] = {
    ( regex("""[^"'\s]\S*""".r) | 
      failure("An unquoted literal may not begin with whitespace or a quotation mark") 
    ) ^^ {
      case string:String => new Literal(string)
    }
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
  val quotedLiteral : Parser[Literal] = {
    ( regex(""""([^\\"]|\\.)*"""".r) | 
      regex("""'([^\\']|\\.)*'""".r) 
    ) ^^ {
      case string:String => {
        val s = 
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
               
         new Literal(s)
      }
    }  
  }
  
    
  /**
   * Parser for a literal value.
   * <p>
   * The literal value may be quoted or unquoted.
   * 
   * @see quotedLiteral
   * @see unquotedLiteral
   */
  val literalValue : Parser[Literal] = {
    unquotedLiteral | quotedLiteral
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
  def name(title:String,whatCanComeNext:Regex): Parser[String] = {
    ( // If the name starts with an illegal character, bail out and don't backtrack
      regex("""[^A-Za-z_]""".r)<~err("Illegal character at start of " + title + " name")

      // Else if the name contains only legal characters and the input ends, then parse it
      | regex("""[A-Za-z_][A-Za-z0-9_]*$""".r)
      
      // Else if the name itself is OK, but it is followed by something that can't legally follow the name, bail out and don't backtrack
      | regex("""[A-Za-z_][A-Za-z0-9_]*""".r)<~guard(not(regex(whatCanComeNext)))~!err("Illegal character in " + title + " name")

      // Finally, if the name contains only legal characters, 
      //          and is followed by something that's allowed to follow it, then parse it!
      | regex("""[A-Za-z_][A-Za-z0-9_]*""".r)
    )
  }
  
  /** Name of a task, enclosed in square brackets. 
   *  <p>
   *  The name must conform to Bash variable name requirements: 
   *  "A word consisting solely of letters, numbers, and underscores, and beginning with a letter or underscore."
   */
  def taskName: Parser[String] = {
    ( // Fail if we have opening and closing brackets, but no task name
      (regex("""\[\s*\]""".r)~!err("Missing task name. Task name must be enclosed in square brackets.")) |
      // Fail if we have whitespace following opening bracket
      (regex("""\[\s+""".r)~!err("Illegal whitespace following opening bracket. Task name must be enclosed in square brackets, with no white space surrounding the task name.")) |
      // Fail if we have no opening bracket 
      (literal("[") | err("Missing opening bracket. Task name must be enclosed in square brackets."))
      // Recognize a name
    ) ~> name("task","""[\s\]]""".r) <~ 
    ( // Fail if we have whitespace following task name
      (regex("""\s+]""".r)~!err("Illegal whitespace following task name. Task name must be enclosed in square brackets, with no white space surrounding the task name.")) |
      // Fail if we have whitespace and no closing bracket
      (regex("""\s+""".r)~!err("Missing closing bracket; illegal whitespace following task name. Task name must be enclosed in square brackets, with no white space surrounding the task name.")) |
      // Fail if we have opening bracket and task name, but not closing bracket
      literal("]") | err("Missing closing bracket. Task name must be enclosed in square brackets.") 
    )
  }
  
  /**
   * Name of a branch point, followed by a colon.
   * <p>
   * Whitespace may optionally separate the name and the colon.
   */
  def branchPointName: Parser[String] = {
    val whatComesNext = """\s*:""".r
    name("branch point",whatComesNext) <~ (regex(whatComesNext) | err("Missing colon after branch point name"))
  }
  
  /**
   * Reference to a variable, 
   * defined as a literal dollar sign ($) followed by a name.
   */
  def variableReference: Parser[Variable] = positioned(
    literal("$")~>name("variable","""\s*""".r) ^^ {
      case string:String => new Variable(string)
    }
  )
  
  /**
   * Reference to a branch name or a branch glob (*)
   */
  def branchReference: Parser[String] = {
    literal("*") | name("branch reference","""[\]\s,]""".r)
  }
  
  /**
   * Branch graft element, 
   * representing a branch point name 
   * and an associated branch reference.
   */
  def branchGraftElement: Parser[BranchGraftElement] = positioned(
      branchPointName~branchReference ^^ {
        case ((a:String) ~ (b:String)) => new BranchGraftElement(a,b)
      }
  )
  
  /**
   * Branch graft, representing a variable name, 
   * a task name, and a list of branch graft elements.
   */
  def branchGraft: Parser[BranchGraft] = positioned(
      (literal("$")~>name("variable","""@""".r)<~literal("@")) ~
      name("task","""\[""".r) ~
      (literal("[")~>rep1sep(branchGraftElement,literal(","))<~literal("]")) ^^ {
        case ((variable:String) ~ (task:String) ~ (seq:Seq[BranchGraftElement])) =>
          new BranchGraft(variable,task,seq)
      } 
  )
  
  def sequentialBranchPoint : Parser[SequentialBranchPoint] = positioned(
      ( regex("""\(\s*""".r) ~> (
          (branchPointName<~regex("""\s*""".r)) ~
          (number<~literal("..")) ~ 
          (number) ~
          opt(literal("..")~>number)) <~
        regex("""\s*\)""".r)
      ) ^^ {
        case ((bpName:String)~(start:BigDecimal)~(end:BigDecimal)~(Some(increment:BigDecimal))) =>
          new SequentialBranchPoint(bpName,start,end,increment)
        case ((bpName:String)~(start:BigDecimal)~(end:BigDecimal)~(None)) =>
          new SequentialBranchPoint(bpName,start,end,new BigDecimal("1"))
      }
  )
  
  
  def taskBlock: Parser[TaskDefinition] = positioned(taskName ^^ {
    case string => new TaskDefinition(string)
  })
  
  def tape: Parser[Tape] = positioned(rep(taskBlock) ^^ {
    case sequence => new Tape(sequence)
  })

}
