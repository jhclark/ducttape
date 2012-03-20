package ducttape.syntax

import java.io.File
import java.math.BigDecimal
import java.math.BigInteger

import org.apache.commons.lang3.StringEscapeUtils;

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
  
  /** One or more whitespace characters */
  val whitespace: Parser[String] = regex("""\s+""".r)
  
  /** One line of comments */
  val comment: Parser[String] = opt(space)~>literal("//")~>regex("""[^\r\n]*""".r)<~eol
  
  /** One or more lines of comments */
  val comments: Parser[Comments] = {
    repsep(comment,opt(space))
//    repsep(comment,opt(space)~eol~opt(space))<~eol
  } ^^ {
    case list:List[String] => {
      if (list.isEmpty) {
        new Comments(None)
      } else {
        new Comments(Some(list.mkString("\n")))
      } 
    }
  }
  
  
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
   * these characters may be any character except 
   * line break (\n), carriage return (\r), or
   * the type of quotation mark being used.
   *  
   * Note that the last character between the quotation marks 
   * may not be an unescaped slash (\), 
   * as this would cause the final quotation mark to be escaped.
   * <p>
   * The quoted text may contain escaped sequences.
   * In the string returned by the parser, any such escaped sequences will be expanded
   * according to the rules for unescaping Java String literals.
   * This is currently implemented using 
   * <code>org.apache.commons.lang3.StringEscapeUtils.unescapeJava</code>
   * from the Apache Commons Lang package.
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
          // Unescape escape codes according to Java rules
          // as implemented in the Apache Commons Lang package
          StringEscapeUtils.unescapeJava(
            string.substring(1,string.length()-1)
          )
               
        new Literal(s)
      }
    }  
  }
  
  /**
   * Parser for a literal value that is wrapped in triple quotes.
   * <p>
   * An triple quoted literal is defined as a string 
   * whose first 3 characters are double quotation marks
   * and whose last 3 characters are double quotation marks.
   * 
   * <p>
   * If there are any characters between the opening and closing sets of quotation marks,
   * these characters may be any character except the type of quotation mark being used,
   * with the caveat that no more than two double quotation mark characters 
   * may immediately precede the closing three double quotation marks.
   * <p>
   * If the quoted text contain any escaped sequences,
   * the literal values of sequences are used.
   * In other words, escape sequences are not expanded. 
   */  
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
    literal("$")~>(name("variable","""\s|\)|$""".r)|err("Missing variable name")) ^^ {
      case string:String => new Variable(string)
    }
  )

  /**
   * Reference to a variable attached to a specific task, 
   * defined as a literal dollar sign ($) followed by a name.
   */
  val taskVariableReference: Parser[TaskVariable] = positioned(
    literal("$")~>name("task variable","""@""".r,err(_),failure(_))~(literal("@")~>name("task name","""\s|\)|$""".r)) ^^ {
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
      (literal("$")~>name("branch graft variable","""@""".r,err(_),failure(_))<~literal("@")) ~
      name("reference to task","""\[""".r,err(_),failure(_)) ~
      (literal("[")~>(rep1sep(branchGraftElement,literal(","))|err("Error while reading branch graft. This indicates one of three things: (1) You left out the closing bracket, or (2) you have a closing bracket, but there's nothing between opening and closing brackets, or (3) you have opening and closing brackets, and there's something between them, but that something is improperly formatted"))<~(literal("]")|error("Missing closing bracket"))) ^^ {
        case ((variable:String) ~ (task:String) ~ (seq:Seq[BranchGraftElement])) =>
          new BranchGraft(variable,task,seq)
      } 
  )
  
  val sequentialBranchPoint : Parser[SequentialBranchPoint] = positioned(
      ( // Must start with an opening parenthesis, then optionally whitespace
        (literal("(")~opt(whitespace))~>
        (  opt( // Then (optionally) the branch point name
             name("branch point",""":""".r,failure(_),failure(_))<~
             // and the colon
             literal(":")
           )<~
           // Then optionally whitespace
           opt(whitespace)
        )~
        ( // First number
          number<~
          // Then dots
          literal("..")
        ) ~
        // Second number
        (number)~
        // Optionally
        opt( // dots
            literal("..")~>
            // then third number
            number)
        ) <~
        ( // Optionally whitespace
            opt(whitespace)~
            // Then closing parenthesis
            literal(")"
        )
      ) ^^ {
        case ((bpName:Option[String])~(start:BigDecimal)~(end:BigDecimal)~(Some(increment:BigDecimal))) =>
          new SequentialBranchPoint(bpName,start,end,increment)      
        case ((bpName:Option[String])~(start:BigDecimal)~(end:BigDecimal)~(None)) =>
          new SequentialBranchPoint(bpName,start,end,new BigDecimal("1"))
      }
  )
  
  
  /** Branch point declaration. */
  val branchPoint : Parser[BranchPointDef] = positioned(
    ( // Must start with an opening parenthesis, then optionally whitespace
      (literal("(")~opt(whitespace))~>
      (  opt( // Then (optionally) the branch point name
           name("branch point",""":""".r,failure(_),failure(_))<~
           // and the colon
           literal(":")
         )<~
         // Then optionally whitespace
         opt(whitespace)
      )~
      // Then the branch assignments or rvalues
      rep1sep(branchAssignment,whitespace)<~
      ( // Optionally whitespace
          opt(whitespace)~
          // Then closing parenthesis
          literal(")")
      )
    ) ^^ {
      case branchPointName ~ seq => new BranchPointDef(branchPointName,seq)
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
  val inputAssignment = basicAssignment("input",failure(_),err(_),err(_))
  
  /** Output variable declaration. */
  val outputAssignment: Parser[Spec] = positioned(
      ( //guard(literal("{")~failure("Unexpectedly encountered opening { brace")) |
          name("output variable","""[=\s]|\z""".r,failure(_),err(_)) ~ 
        opt("=" ~> (rvalue | err("Error in output variable assignment")))
      ) ^^ {
        case (variableName:String) ~ Some(rhs:RValue) => new Spec(variableName,rhs,false)
        case (variableName:String) ~ None             => new Spec(variableName,Unbound(),false)
      }      
  )
    
  /** Parameter variable declaration. */
  val paramAssignment: Parser[Spec] = positioned(
      ( //guard(literal("{")~failure("Unexpectedly encountered opening { brace")) |
          opt(literal("."))~(name("parameter variable","""[=\s]|\z""".r,failure(_),err(_)) <~ "=") ~ 
        (rvalue | err("Error in parameter variable assignment"))
      ) ^^ {
        case Some(_:String) ~ (variableName:String) ~ (rhs:RValue) => new Spec(variableName,rhs,true)
        case None           ~ (variableName:String) ~ (rhs:RValue) => new Spec(variableName,rhs,false)
      }      
  )
  


  /**
   * Sequence of <code>assignment</code>s representing input files.
   * This sequence must be preceded by "<".
   */
  val taskInputs: Parser[TaskInputs] = {
    ( // Comments describe the input block
      //   There may not be any comments,
      //   in which case the comments object
      //   will contain an empty string
        ( comments<~
          // there may be whitespace after the comments    
          opt(whitespace)~
          // then there must be a < character  
          literal("<") ~ 
          // then one or more spaces or tabs
          space
         ) ~ 
         // Finally the list of input assignments
         repsep(inputAssignment, space)
    ) | failure("Failed to parse task inputs")
  } ^^ {
    case comments~list => new TaskInputs(list,comments)
  }

  /**
   * Sequence of <code>assignment</code>s representing output files.
   * This sequence must be preceded by ">".
   *
   */
  val taskOutputs: Parser[TaskOutputs] = {
    ( // Comments describe the output block
      //   There may not be any comments,
      //   in which case the comments object
      //   will contain an empty string
        ( comments<~
          // there may be whitespace after the comments    
          opt(whitespace)~
          // then there must be a > character  
          literal(">") ~ 
          // then one or more spaces or tabs
          space
         ) ~ 
         // Finally the list of input assignments
         repsep(outputAssignment, space)
    ) | failure("Failed to parse task outputs")
  } ^^ {
    case comments~list => new TaskOutputs(list,comments)    
  }

  /**
   * Sequence of <code>assignment</code>s representing parameter values.
   * This sequence must be preceded by "::".
   */
  val taskParams: Parser[TaskParams] = { //opt("::" ~ rep(space) ~> repsep(paramAssignment, space)) ^^ {
    ( // Comments describe the parameter block
      //   There may not be any comments,
      //   in which case the comments object
      //   will contain an empty string
        ( comments<~
          // there may be whitespace after the comments    
          opt(whitespace)~
          // then there must be :: characters  
          literal("::") ~ 
          // then one or more spaces or tabs
          space
         ) ~ 
         // Finally the list of input assignments
         repsep(paramAssignment, space)
    ) | failure("Failed to parse task parameters")
  } ^^ {
    case comments~list => new TaskParams(list,comments)
  }  


  val taskSpec:Parser[Specs] = {
    taskInputs | taskOutputs | taskParams //| failure("Failed to parse task spec")
  }
    
  val taskSpecs:Parser[List[Specs]] = {
    repsep(taskSpec,regex("""[ \n\r\t]+""".r)) //| failure("Dude")
  }  
  
  val packageNames:Parser[PackageNames] = {
    (comments <~ opt(eol)~opt(space))~ repsep(name("package","""\s""".r,failure(_),err(_)),space)    
//    (comments <~ opt(eol ~ space))~ repsep(name("package","""\s""".r,failure(_),err(_)),space)
  } ^^ {
    case (comments:Comments) ~ (packageNames:List[String]) => new PackageNames(comments,packageNames)
  }
  
  val taskHeader:Parser[TaskHeader] = {
    (packageNames <~ (opt(space)~opt(eol)~opt(space)))~ taskSpecs
  } ^^ {
    case (packageNames:PackageNames) ~ (specs:List[Specs]) =>
      new TaskHeader(packageNames,specs) 
  }
  
  val taskBlock: Parser[TaskDefinition] = positioned({
    (
        comments //<~ 
//        (
//            opt(whitespace) ~
//            opt(eol)
//        )
    ) ~
    taskName ~ 
    (
        whitespace ~>
        taskHeader
    ) ~ 
    (
        (
            opt(whitespace) ~
            (
                literal("{") |
                err("Missing opening { brace.")
            ) ~
            opt(space) ~
            (
                eol |
                err("Shell commands may not start on the same line as the opening { brace.")
            )
        ) ~> 
        shellCommands <~
        (
            eol ~ 
            (
                literal("}") ~ 
                (
                    opt(space) ~
                    guard(eol | err("Non-whitespace character found following closing } brace."))
                ) |                
                space~literal("}")~err("Closing } brace may not be preceded by whitespace.") |
                err("Missing closing } brace. If you have a closing brace but still got error message anyway, it probably means that you have have whitespace preceding your closing } brace. The closing } brace must be the first character of the line - it may not be preceded by any whitespace.")
            )
        )
    ) 
  } ^^ {
    case (comments:Comments) ~ (name:String) ~ (header:TaskHeader) ~ (commands:ShellCommands) => 
      new TaskDefinition(comments,name,header,commands)
  })

  
  def shellCommands: Parser[ShellCommands] = positioned(
    repsep(shellCommand,"""(\r\n)|\r|\n""".r) ^^ {
      case list:List[String] => new ShellCommands(list.mkString("\n"))
    }
  )
  
  /** Shell command. */
  def shellCommand: Parser[String] = {
    (regex("""[^}\n\r][^\r\n]*""".r) <~ guard(eol)) |
    guard(eol) |
    failure("The first character of a shell script line may not be }. You can fix this error by preceding the } with one or more whitespace characters.")
  }

    
  val tape: Parser[Tape] = positioned(rep(taskBlock) ^^ {
    case sequence => new Tape(sequence)
  })

}
