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
          (regex("""[+-]?\.\d+([eE][-+]?\d+)?""".r)~!err("A number must have at least one digit left of the decimal point.") )
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
    ( 
      // NOTE: If we encounter whitespace we MUST allow backtracking, so we call failure instead of err  
      (regex("""[^\s]*\s""".r)~failure("An unquoted literal may not contain whitespace")) |
      (regex("""[^@\s]*@""".r)~err("An unquoted literal may not contain an @ symbol. If this was meant to be a variable reference instead of an unquoted literal, then you probably forgot the $ sign.")) |      
      (regex("""[^"\s]*["]""".r)~err("An unquoted literal may not contain a double quotation mark")) |
      (regex("""[^'\s]*[']""".r)~err("An unquoted literal may not contain a single quotation mark")) |      
      (regex("""[^\[\s]*[\[]""".r)~err("An unquoted literal may not contain an opening square bracket")) |      
      (regex("""[^\]\s]*[\]]""".r)~err("An unquoted literal may not contain a closing square bracket")) |        
      (regex("""[^:\s]*:""".r)~err("An unquoted literal may not contain a colon")) |      
      (regex("""[^\*\s]*\*""".r)~err("An unquoted literal may not contain a * symbol")) |
      (regex("""[^\$\s]*\$""".r)~err("An unquoted literal may not contain a $ symbol")) |
      (regex("""[^(\s]*[(]""".r)~err("An unquoted literal may not contain an opening parenthesis")) |      
      // NOTE: If we encounter a closing parenthesis we MUST allow backtracking, so we call failure instead of err
      (regex("""[^)\s]*[)]""".r)~failure("An unquoted literal may not contain a closing parenthesis")) |  
      regex("""[^"')(\]\[\*\$:@=\s]+""".r)
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
    ( ( // A valid double-quoted string
        regex(""""([^\\"]|\\.)*"""".r) |
        // Or, if missing closing quote, an error
        regex(""""([^\\"]|\\.)*""".r)~err("Missing closing quotation mark") |
        // A valid single-quoted string
        regex("""'([^\\']|\\.)*'""".r) |
        // Or, if missing closing quote, an error
        regex("""'([^\\']|\\.)*""".r)~err("Missing closing quotation mark") 
      ) <~ (regex("$".r)|guard(regex("""[\s)]""".r))|err("A quoted literal may not continue after the closing quotation mark"))
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
         //     expand escaped backspace characters
                .replace("""\b""","\b")
         //     expand escaped single quote characters                
               .replace("""\'""","'")
         //     expand escaped double quote characters                
               .replace("""\"""","\"")               
         //     expand escaped slash characters               
               .replace("""\\""","\\")             
         //TODO expand escaped unicode escapes 
         //     .replaceAll("\\\\u([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f])","$1")
               
         new Literal(s)
      }
    }  
  }
  
  val tripleQuotedLiteral : Parser[Literal] = {
    ( ( // A valid triple-quoted string
        regex("""["]["]["]([^"]["]?["]?)*["]["]["]""".r) |
        // Or, if missing closing quote, an error
        regex("""["]["]["][^"]*["]?["]?""".r)~err("Missing closing triple quotation marks")
      ) <~ (regex("$".r)|guard(regex("""[\s)]""".r))|err("A quoted literal may not continue after the closing quotation mark"))
    ) ^^ {
      case string:String => new Literal(string.substring(3,string.length()-3))
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
    tripleQuotedLiteral | quotedLiteral | unquotedLiteral 
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
    name(title,whatCanComeNext,err(_),err(_))
  }

  /**
   * Parser for a name, defined as an ASCII alphanumeric identifier.
   * <p>
   * The first character must be an upper-case letter, an lower-case letter, or an underscore.
   * Each subsequent character in the name (if any exist) 
   * must be an upper-case letter, a lower-case letter, a numeric digit, or an underscore.
   * 
   * @param whatCanComeNext Regular expression that specifies what may legally follow the name
   * @param howToFailAtEnd Function that defines error or failure behavior to follow when an illegal expression follows the name
   */
  def name(title:String,
      whatCanComeNext:Regex,
      howToFailAtStart:(String)=>Parser[Nothing],
      howToFailAtEnd:(String)=>Parser[Nothing]): Parser[String] = {
    ( // If the name starts with an illegal character, bail out and don't backtrack
      regex("""[^A-Za-z_]""".r)<~howToFailAtStart("Illegal character at start of " + title + " name")

      // Else if the name contains only legal characters and the input ends, then parse it
      | regex("""[A-Za-z_][A-Za-z0-9_]*$""".r)
      
      // Else if the name itself is OK, but it is followed by something that can't legally follow the name, bail out and don't backtrack
      | regex("""[A-Za-z_][A-Za-z0-9_]*""".r)<~guard(not(regex(whatCanComeNext)))~howToFailAtEnd("Illegal character in " + title + " name")

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
  val taskName: Parser[String] = {
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
  val branchPointName: Parser[String] = {
    val whatComesNext = """\s*:""".r
    name("branch point",whatComesNext) <~ (regex(whatComesNext) | err("Missing colon after branch point name"))
  }
  
  /**
   * Reference to a variable, 
   * defined as a literal dollar sign ($) followed by a name.
   */
  val variableReference: Parser[Variable] = positioned(
    literal("$")~>(name("variable","""\s*""".r)|err("Missing variable name")) ^^ {
      case string:String => new Variable(string)
    }
  )

  /**
   * Reference to a variable attached to a specific task, 
   * defined as a literal dollar sign ($) followed by a name.
   */
  val taskVariableReference: Parser[TaskVariable] = positioned(
    literal("$")~>name("variable","""\s*""".r)~(literal("@")~>name("task name","""\s*""".r)) ^^ {
      case (string:String) ~ (taskName:String) => new TaskVariable(taskName,string)
    }
  )  
  
  /**
   * Reference to a branch name or a branch glob (*)
   */
  val branchReference: Parser[String] = {
    literal("*") | name("branch reference","""[\]\s,]""".r)
  }
  
  /**
   * Branch graft element, 
   * representing a branch point name 
   * and an associated branch reference.
   */
  val branchGraftElement: Parser[BranchGraftElement] = positioned(
      branchPointName~branchReference ^^ {
        case ((a:String) ~ (b:String)) => new BranchGraftElement(a,b)
      }
  )
  
  /**
   * Branch graft, representing a variable name, 
   * a task name, and a list of branch graft elements.
   */
  val branchGraft: Parser[BranchGraft] = positioned(
      (literal("$")~>name("variable","""@""".r)<~literal("@")) ~
      name("reference to task","""\[""".r,err(_),failure(_)) ~
      (literal("[")~>(rep1sep(branchGraftElement,literal(","))|err("Error while reading branch graft. This indicates one of three things: (1) You left out the closing bracket, or (2) you have a closing bracket, but there's nothing between opening and closing brackets, or (3) you have opening and closing brackets, and there's something between them, but that something is improperly formatted"))<~(literal("]")|error("Missing closing bracket"))) ^^ {
        case ((variable:String) ~ (task:String) ~ (seq:Seq[BranchGraftElement])) =>
          new BranchGraft(variable,task,seq)
      } 
  )
  
  val sequentialBranchPoint : Parser[SequentialBranchPoint] = positioned(
      ( regex("""\(\s*""".r) ~> (
          ((name("branch point",""":""".r,failure(_),failure(_))<~literal(":"))<~regex("""\s*""".r)) ~
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
  
  val rvalue : Parser[RValue] = {
    sequentialBranchPoint |
    branchPoint           |    
    branchGraft           |
    taskVariableReference |
    variableReference     |
    // Order is important here. 
    // Only try parsing as a literal if it's definitely not something else. 
    literalValue          |
    (regex("""(\s*\z)|\s+""".r)~>err("An rvalue may not be empty"))
  }


  def basicAssignment(variableType:String,
                      howToFailAtStart:(String)=>Parser[Nothing],
                      howToFailAtEnd:(String)=>Parser[Nothing],
                      howToFailAtEquals:(String)=>Parser[Nothing]): Parser[Spec] = positioned(
      ( ( // First, a variable name
          name(variableType + " variable","""[=\s]|\z""".r,howToFailAtStart,howToFailAtEnd) <~ 
          ( // Next, the equals sign
            literal("=") | 
            // Or an error if the equals sign is missing
            howToFailAtEquals(variableType + " variable assignment is missing equals sign and right-hand side")
          )
        ) ~
        ( // Next, the right-hand side
          rvalue | 
          // Or an error
          err("Error in input variable assignment")
        )
      ) ^^ {
        case (variableName:String) ~ (rhs:RValue) => new Spec(variableName,rhs,false)
      }      
  )

  val branchAssignment:Parser[Spec] = positioned(
      (basicAssignment("branch",failure(_),failure(_),failure(_)) | rvalue) ^^ {
        case assignment:Spec => assignment
        case rhs:RValue      => new Spec(null,rhs,false)
      }
  )

  /** Input variable declaration. */  
  val inputAssignment = basicAssignment("input",err(_),err(_),err(_))
  
  /** Output variable declaration. */
  val outputAssignment: Parser[Spec] = positioned(
      ( name("output variable","""[=\s]|\z""".r) ~ 
        opt("=" ~> (rvalue | err("Error in output variable assignment")))
      ) ^^ {
        case (variableName:String) ~ Some(rhs:RValue) => new Spec(variableName,rhs,false)
        case (variableName:String) ~ None             => new Spec(variableName,Unbound(),false)
      }      
  )
    
  /** Parameter variable declaration. */
  val paramAssignment: Parser[Spec] = positioned(
      ( opt(literal("."))~(name("parameter variable","""[=\s]|\z""".r) <~ "=") ~ 
        (rvalue | err("Error in parameter variable assignment"))
      ) ^^ {
        case Some(_:String) ~ (variableName:String) ~ (rhs:RValue) => new Spec(variableName,rhs,true)
        case None           ~ (variableName:String) ~ (rhs:RValue) => new Spec(variableName,rhs,false)
      }      
  )
  
  
  val taskVariableAssignments = repsep((taskInputs | taskOutputs | taskParams),space)
  
  /**
   * Sequence of <code>assignment</code>s representing input files.
   * This sequence must be preceded by "<".
   */
  def taskInputs: Parser[Seq[Spec]] = opt("<" ~ rep(space) ~> repsep(inputAssignment, space)) ^^ {
    case Some(list) => list
    case None => List.empty
  }

  /**
   * Sequence of <code>assignment</code>s representing output files.
   * This sequence must be preceded by ">".
   *
   */
  def taskOutputs: Parser[Seq[Spec]] = opt(">" ~ rep(space) ~> repsep(outputAssignment, space)) ^^ {
    case Some(list) => list
    case None => List.empty
  }

  /**
   * Sequence of <code>assignment</code>s representing parameter values.
   * This sequence must be preceded by "::".
   */
  def taskParams: Parser[Seq[Spec]] = opt("::" ~ rep(space) ~> repsep(paramAssignment, space)) ^^ {
    case Some(params) => params
    case None => List.empty
  }  
  
  /** Branch point declaration. */
  def branchPoint : Parser[BranchPointDef] = positioned(
    ( // Must start with an opening parenthesis, then optionally whitespace
      (literal("(")~opt(space))~>
      (  opt( // Then (optionally) the branch point name
           name("branch point",""":""".r,failure(_),failure(_))<~
           // and the colon
           literal(":")
         )<~
         // Then optionally whitespace
         opt(space)
      )~
      // Then the branch assignments or rvalues
      rep1sep(branchAssignment,space)<~
      // Finally optional space, then the closing parenthesis
      (opt(space)~literal(")"))
    ) ^^ {
      case Some(branchPointName) ~ seq => new BranchPointDef(branchPointName,seq)
      case None                  ~ seq => new BranchPointDef(null,seq)
    }
  )
  
  val taskBlock: Parser[TaskDefinition] = positioned(taskName ^^ {
    case string => new TaskDefinition(string)
  })
  
  val tape: Parser[Tape] = positioned(rep(taskBlock) ^^ {
    case sequence => new Tape(sequence)
  })

}
