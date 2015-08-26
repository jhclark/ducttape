// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax

import java.io.File
import java.math.BigDecimal
import org.apache.commons.lang3.StringEscapeUtils

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import scala.util.parsing.input.NoPosition

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.cli.ErrorUtils
import ducttape.util.Files

object Grammar {
  import ducttape.syntax.GrammarParser._ // we need visibility of Parser, etc.

  /** End of line characters, including end of file. */
  val eol: Parser[String] = literal("\r\n") | literal("\n") | regex("""\z""".r) | literal(CharArrayReader.EofCh.toString)

  /** Non-end of line white space characters */
  val space: Parser[String] = regex("""[ \t]+""".r) | failure("Expected one or more space or tab characters, but didn't find it here")

  /** One or more whitespace characters */
  val whitespace: Parser[String] = regex("""\s+""".r) | failure("Expected whitespace but didn't find it here")

  object Keyword {

    private def keyword(word: String, typeOfWhitespace: Parser[Any] = space): Parser[String] = {
      (
          literal(word) |
          failure("""The keyword """"+word+"""" is required but missing.""")
      ) <~
      (
          typeOfWhitespace |
          err("""One or more whitespace characters must immediately follow the """"+word+"""" keyword.""")
      )
    }

    val task: Parser[String] = keyword("task")
    val group: Parser[String] = keyword("group")
    val func: Parser[String] = keyword("func")
    val calls: Parser[String] = keyword("calls")
    val summmary: Parser[String] = keyword("summary")
    val of: Parser[String] = keyword("of")
    val action: Parser[String] = keyword("action")
    val submitter: Parser[String] = keyword("submitter")
    val versioner: Parser[String] = keyword("versioner")
    val packageKeyword: Parser[String] = keyword("package")
    val branchpoint: Parser[String] = keyword("branchpoint")
    val baseline: Parser[String] = keyword("baseline")
    val branch: Parser[String] = keyword("branch")
    val config: Parser[String] = keyword("config")
    val reach: Parser[String] = keyword("reach")
    val via: Parser[String] = keyword("via", commentableWhitespace)
    val plan: Parser[String] = keyword("plan")
    val global: Parser[String] = keyword("global")
    val importKeyword: Parser[String] = keyword("import")
    val tabbedTask: Parser[String] = keyword("tabbed task")

  }

  /** One line of comments */
  val lineComment: Parser[String] = {
      opt(space) ~>
      (literal("//") | literal("#")) ~>
      regex("""[^\r\n]*""".r) <~
      eol
  }

  val blockComment: Parser[String] = {
    literal("/*") ~>
    rep(
      not(literal("*/"))~>
      (
        blockComment |
        regex(""".|\n|\r""".r)
      )
    ) <~
    (
      literal("*/") |
      err("Comment is missing closing */")
    )

  } ^^ {
    case list:List[String] => {
      val sb = new StringBuilder()
      list.foreach((item:String) => sb.append(item) )
      sb.toString
    }
  }


  val comment: Parser[String] = lineComment | blockComment

  /** One or more lines of comments */
  val comments: Parser[Comments] = {
    repsep(comment,opt(whitespace)) <~ opt(whitespace)
  } ^^ {
    case list:List[String] => {
      if (list.isEmpty) {
        new Comments(None)
      } else {
        new Comments(Some(list.mkString("\n")))
      }
    }
  }

  val commentableWhitespace: Parser[Comments] = {
    opt(whitespace) ~> comments
  }

  /** A signed, arbitrary precision number. */
  val number: Parser[BigDecimal] =
      ( // Recognize a number with at least one digit left of the decimal
          // and optionally, one or more digits to the right of the decimal
          regex("""[+-]?\d+(\.\d+)?([eE][-+]?\d+)?""".r)  |
          // Do NOT recognize a number with no digits left of the decimal
          (regex("""[+-]?\.\d+([eE][-+]?\d+)?""".r)~!err("A number must have at least one digit left of the decimal point.") )
      ) ^^ {
        case s: String => new BigDecimal(s)
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
  val unquotedLiteral: Parser[Literal] = {
    (
      (regex("""[^@\s]*@""".r)~err("An unquoted literal may not contain an @ symbol. If this was meant to be a variable reference instead of an unquoted literal, then you probably forgot the $ sign.")) |
      (regex("""[^"\s]*["]""".r)~err("An unquoted literal may not contain a double quotation mark")) |
      (regex("""[^'\s]*[']""".r)~err("An unquoted literal may not contain a single quotation mark")) |
      (regex("""[^\[\s]*[\[]""".r)~err("An unquoted literal may not contain an opening square bracket")) |
      (regex("""[^\]\s]*[\]]""".r)~err("An unquoted literal may not contain a closing square bracket")) |
      (regex("""[^:\s]*:""".r)~err("An unquoted literal may not contain a colon")) |
      (regex("""[^*\s]*\*""".r)~err("An unquoted literal may not contain a * symbol")) |
      (regex("""[^+\s]*\+""".r)~err("An unquoted literal may not contain a + symbol")) |
      (regex("""[^$\s]*\$""".r)~err("An unquoted literal may not contain a $ symbol")) |
      (regex("""[^(\s]*[(]""".r)~err("An unquoted literal may not contain an opening parenthesis")) |
      // NOTE: If we encounter a closing parenthesis we MUST allow backtracking, so we call failure instead of err
      (regex("""[^)\s]*[)]""".r)~failure("An unquoted literal may not contain a closing parenthesis")) |
      // NOTE: If we encounter whitespace we MUST allow backtracking, so we call failure instead of err
      (regex("""[^\s]*\s""".r)~failure("An unquoted literal may not contain whitespace. If you meant to refer to a variable, you probably forgot the $ sign.")) |
      regex("""[^"')(\]\[*+$:@=\s]+""".r)
    ) ^^ {
      case string: String => new Literal(string)
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
  val quotedLiteral: Parser[Literal] = {
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
      case string: String => {
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
  val tripleQuotedLiteral: Parser[Literal] = {
    ( ( // A valid triple-quoted string
        regex("""["]["]["]([^"]["]?["]?)*["]["]["]""".r) |
        // Or, if missing closing quote, an error
        regex("""["]["]["][^"]*["]?["]?""".r)~err("Missing closing triple quotation marks")
      ) <~ (regex("$".r)|guard(regex("""[\s)]""".r))|err("A quoted literal may not continue after the closing quotation mark"))
    ) ^^ {
      case string: String => new Literal(string.substring(3,string.length()-3))
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
  val literalValue: Parser[Literal] = {
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
  def name(title: String, whatCanComeNext: Regex): Parser[String] = {
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
  def name(title: String,
      whatCanComeNext: Regex,
      howToFailAtStart: (String)=>Parser[Nothing],
      howToFailAtEnd: (String)=>Parser[Nothing]): Parser[String] = {
    ( // If the name starts with an illegal character, bail out and don't backtrack
      regex("""[^A-Za-z_]""".r)<~howToFailAtStart("Illegal character at start of " + title + " name")

      // Else if the name contains only legal characters and the input ends, then parse it
      | regex("""[A-Za-z_][A-Za-z0-9_]*$""".r)

      // Else if the name itself is OK, but it is followed by something that can't legally follow the name, bail out and don't backtrack
      | regex("""[A-Za-z_][A-Za-z0-9_]*""".r)<~guard(not(regex(whatCanComeNext)))~howToFailAtEnd("Illegal character in " + title + " name. Adding a space after the variable name may fix this error.")

      // Finally, if the name contains only legal characters,
      //          and is followed by something that's allowed to follow it, then parse it!
      | regex("""[A-Za-z_][A-Za-z0-9_]*""".r)
    )
  }

  /**
   * A convenience method for branchName() like name() above
   */
  def branchName(title: String, whatCanComeNext: Regex): Parser[String] = {
    branchName(title,whatCanComeNext,err(_),err(_))
  }

  /**
   * Branch name, with the same naming restrictions as unquoted literals.
   */
  def branchName(title: String,
      whatCanComeNext: Regex,
      howToFailAtStart: (String)=>Parser[Nothing],
      howToFailAtEnd: (String)=>Parser[Nothing]): Parser[String] = {
    ( // If the name starts with an illegal character, bail out and don't backtrack
      regex("""[^A-Za-z0-9_\-.]""".r)<~howToFailAtStart("Illegal character at start of " + title + " name")

      // Else if the name contains only legal characters and the input ends, then parse it
      | regex("""[A-Za-z0-9_\-.]*$""".r)

      // Else if the name itself is OK, but it is followed by something that can't legally follow the name, bail out and don't backtrack
      | regex("""[A-Za-z0-9_\-.]*""".r)<~guard(not(regex(whatCanComeNext)))~howToFailAtEnd("Illegal character in " + title + " name. Adding a space after the variable name may fix this error.")

      // Finally, if the name contains only legal characters,
      //          and is followed by something that's allowed to follow it, then parse it!
      | regex("""[A-Za-z0-9_\-.]*""".r)
    )
  }

  /** Convenient shorthand type that encompasses name and branchName */
  type NameParserType = (String, Regex, String=>Parser[Nothing], String=>Parser[Nothing]) => Parser[String]




  val name: Parser[String] = {
    name("task","""\s""".r)
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
  val variableReference: Parser[ConfigVariable] = positioned(
    ( ((literal("$")~literal("{"))~>(name("variable","""}""".r)|err("Missing variable name"))<~(literal("}")|err("Missing closing } brace"))) |
      (literal("$")~>(name("variable","""\s|\)|$""".r)|err("Missing variable name")))
    )^^ {
      case string:String => new ConfigVariable(string)
    }
  )

  /**
   * Reference to a variable attached to a specific task,
   * defined as a literal dollar sign ($) followed by a name.
   */
  val taskVariableReference: Parser[TaskVariable] = positioned(
    ( ((literal("$")~literal("{"))~>name("task variable","""}""".r,err(_),failure(_))~((literal("}")|err("Missing closing } brace"))~literal("@")~>name("task","""\s|\)|$""".r))) |
      (literal("$")~>name("task variable","""@""".r,err(_),failure(_))~(literal("@")~>name("task","""\s|\)|$""".r)) )
    ) ^^ {
      case (string: String) ~ (taskName: String) => new TaskVariable(taskName,string)
    }
  )

  /**
   * Shorthand reference to a variable attached to a specific task,
   * defined as a literal at symbol (@) followed by a name.
   */
  val shorthandTaskVariableReference: Parser[ShorthandTaskVariable] = positioned(
    ( literal("@")~>name("task","""\s|\)|$""".r,failure(_),err(_))

    ) ^^ {
      case (taskName: String) => new ShorthandTaskVariable(taskName)
    }
  )
  /**
   * Shorthand reference to a variable,
   * defined as a literal at symbol (@).
   */
  val shorthandVariableReference: Parser[ShorthandConfigVariable] = positioned(
    literal("@") ^^ {
      case _ => new ShorthandConfigVariable()
    }
  )

  /**
   * Reference to a branch name or a branch glob (*)
   */
  val branchReference: Parser[String] = {
    literal("*") | branchName("branch reference","""[\]\s,]""".r)
  }

  /**
   * Branch graft element,
   * representing a branch point name
   * and an associated branch reference.
   */
  val branchGraftElement: Parser[BranchGraftElement] = positioned(
      branchPointName~branchReference ^^ {
        case ((a: String) ~ (b: String)) => new BranchGraftElement(a,b)
      }
  )

  /**
   * Branch graft, representing a variable name,
   * a task name, and a list of branch graft elements.
   */
  val branchGraft: Parser[BranchGraft] = positioned(
      (
          (
              (literal("$")~literal("{")~>name("branch graft variable","""}""".r,err(_),failure(_))<~(literal("}")|err("Missing closing } brace"))~literal("@")) ~
              name("reference to task","""\[""".r,err(_),failure(_)) ~
              (literal("[")~>(rep1sep(branchGraftElement,literal(","))|err("Error while reading branch graft. This indicates one of three things: (1) You left out the closing bracket, or (2) you have a closing bracket, but there's nothing between opening and closing brackets, or (3) you have opening and closing brackets, and there's something between them, but that something is improperly formatted"))<~(literal("]")|err("Missing closing bracket")))
          ) |
          (
              (literal("$")~>name("branch graft variable","""@""".r,err(_),failure(_))<~literal("@")) ~
              name("reference to task","""\[""".r,err(_),failure(_)) ~
              (literal("[")~>(rep1sep(branchGraftElement,literal(","))|err("Error while reading branch graft. This indicates one of three things: (1) You left out the closing bracket, or (2) you have a closing bracket, but there's nothing between opening and closing brackets, or (3) you have opening and closing brackets, and there's something between them, but that something is improperly formatted"))<~(literal("]")|err("Missing closing bracket")))
          ) |
          (
              (literal("$")~literal("{")~>name("branch graft variable","""}""".r,err(_),failure(_))<~(literal("}")|err("Missing closing } brace"))) ~
              (literal("[")~>(rep1sep(branchGraftElement,literal(","))|err("Error while reading branch graft. This indicates one of three things: (1) You left out the closing bracket, or (2) you have a closing bracket, but there's nothing between opening and closing brackets, or (3) you have opening and closing brackets, and there's something between them, but that something is improperly formatted"))<~(literal("]")|err("Missing closing bracket")))
          ) |
          (
              (literal("$")~>name("branch graft variable","""@""".r,err(_),failure(_))) ~
              (literal("[")~>(rep1sep(branchGraftElement,literal(","))|err("Error while reading branch graft. This indicates one of three things: (1) You left out the closing bracket, or (2) you have a closing bracket, but there's nothing between opening and closing brackets, or (3) you have opening and closing brackets, and there's something between them, but that something is improperly formatted"))<~(literal("]")|err("Missing closing bracket")))
          )
      ) ^^ {
        case ((variable: String) ~ (task: String) ~ (seq: Seq[BranchGraftElement])) =>
          new BranchGraft(variable,Some(task),seq)
        case ((variable: String) ~ (seq: Seq[BranchGraftElement])) =>
          new BranchGraft(variable,None,seq)
      }
  )

  val shorthandBranchGraft: Parser[ShorthandBranchGraft] = positioned(
      (
          (
              literal("@[")~err("In the interest of readability, shorthand syntax for branch grafts involving global variables and config variables is currently not allowed.")
          ) |
          (
              literal("@") ~>
              name("reference to task","""\[""".r,failure(_),failure(_)) ~
              (literal("[")~>(rep1sep(branchGraftElement,literal(","))|err("Error while reading branch graft. This indicates one of three things: (1) You left out the closing bracket, or (2) you have a closing bracket, but there's nothing between opening and closing brackets, or (3) you have opening and closing brackets, and there's something between them, but that something is improperly formatted"))<~(literal("]")|err("Missing closing bracket")))
          )
      ) ^^ {
        case ((task: String) ~ (seq: Seq[BranchGraftElement])) =>
          new ShorthandBranchGraft(task,seq)
      }
  )

  val sequence: Parser[Sequence] = positioned(
      (
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
            number
        )
      )  ^^ {
        case ((start: BigDecimal)~(end: BigDecimal)~(Some(increment: BigDecimal))) =>
          new Sequence(start,end,increment)
        case ((start: BigDecimal)~(end: BigDecimal)~(None)) =>
          new Sequence(start,end,new BigDecimal("1"))
      }
      )

  val sequentialBranchPoint: Parser[SequentialBranchPoint] = positioned(
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
        sequence <~
        ( // Optionally whitespace
            opt(whitespace)~
            // Then closing parenthesis
            literal(")")
        )
      ) ^^ {
        case ((bpName: Option[String])~(seq: Sequence)) =>
          new SequentialBranchPoint(bpName,seq)
      }
  )


  /** Branch point declaration. */
  val branchPoint: Parser[BranchPointDef] = positioned(
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
      rep1sep(branchAssignment,(whitespace|failure("Expected whitespace after branch assignment, but didn't find it")))<~
      ( // Optionally whitespace
          opt(whitespace)~
          // Then closing parenthesis
          literal(")")
      )
    ) ^^ {
      case branchPointName ~ seq => new BranchPointDef(branchPointName,seq)
    }
  )

  val branchPointRefBranchName: Parser[ASTType] = {
    sequence |
    literalValue
  }

  def branchPointRef(typeOfWhitespace: Parser[Any]): Parser[BranchPointRef] = positioned({
    ( // Must start with an opening parenthesis, then optionally whitespace
      (literal("(")~opt(typeOfWhitespace))~>
      (  (// Then (optionally) the branch point name
           name("branch point",""":""".r,failure(_),failure(_))<~
           // and the colon
           literal(":")
         )<~
         // Then optionally whitespace
         opt(typeOfWhitespace)
      ) ~
      (
          literal("*") |
          repsep(
              branchPointRefBranchName,
              regex("""\s""".r)~opt(typeOfWhitespace)
          )
      ) <~
      ( opt(typeOfWhitespace) ~ literal(")")
      )

    )
    } ^^ {
      case (bpName: String) ~ (branchName: String) => new BranchPointRef(bpName,List.apply(new Literal(branchName)))
      case (bpName: String) ~ (branchNames: List[ASTType @unchecked]) => new BranchPointRef(bpName,branchNames)
    }
  )

  val crossProduct: Parser[CrossProduct] = positioned({
    commentableWhitespace ~>
    (
        Keyword.reach ~>
        rep1sep(name("goal","""[\s,]""".r),opt(space)~literal(",")~opt(space))
    ) ~
    opt(
        (
            space ~
            Keyword.via ~
            opt(commentableWhitespace)
        ) ~>
        (
            rep1sep(branchPointRef(commentableWhitespace),opt(commentableWhitespace)~literal("*")~opt(commentableWhitespace)) |
            (
                (literal("{")~opt(commentableWhitespace)) ~>
                rep1sep(branchPointRef(commentableWhitespace),opt(commentableWhitespace)~literal("*")~opt(commentableWhitespace)) <~
                (opt(commentableWhitespace)~(literal("}")|err("Missing closing } bracket.")))
            )
        )
    )
  } ^^ {
    case (goals: Seq[String]) ~ Some(crossProduct: Seq[BranchPointRef]) => new CrossProduct(goals,crossProduct)
    case (goals: Seq[String]) ~ None => new CrossProduct(goals,Seq(new BranchPointRef("Baseline",Seq(new Literal("baseline")))))
  })


  val rvalue: Parser[RValue] = {
    sequentialBranchPoint |
    branchPoint           |
    shorthandBranchGraft  |
    branchGraft           |
    shorthandTaskVariableReference |
    shorthandVariableReference     |
    taskVariableReference |
    variableReference     |
    // Order is important here.
    // Only try parsing as a literal if it's definitely not something else.
    literalValue          |
    (regex("""(\s*\z)|\s+""".r)~>err("An rvalue may not be empty"))
  }


  def basicAssignment(variableType: String,
                      specGenerator: SpecGenerator,
                      howToFailAtStart: (String)=>Parser[Nothing],
                      howToFailAtEnd: (String)=>Parser[Nothing],
                      howToFailAtEquals: (String)=>Parser[Nothing],
                      nameParser: NameParserType = name): Parser[Spec] = positioned(
      ( ( // First, a variable name
          nameParser(variableType + " variable","""[=\s]|\z""".r,howToFailAtStart,howToFailAtEnd) <~
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
        case (variableName: String) ~ (rhs: ShorthandTaskVariable) => specGenerator(variableName,new TaskVariable(rhs.taskName,variableName))
        case (variableName: String) ~ (rhs: ShorthandConfigVariable) => specGenerator(variableName,new ConfigVariable(variableName))
        case (variableName: String) ~ (rhs: ShorthandBranchGraft) => specGenerator(variableName, new BranchGraft(variableName,Some(rhs.taskName),rhs.branchGraftElements))
        case (variableName: String) ~ (rhs: RValue) => specGenerator(variableName,rhs)
      }
  )

  val branchAssignment: Parser[Spec] = positioned(
      (basicAssignment("branch",branchSpecGenerator,failure(_),failure(_),failure(_),branchName) | rvalue) ^^ (
          (x:ASTType) => {
            val spec:Spec = x match {
              case assignment: AbstractSpec[_] => assignment
              case _: ShorthandTaskVariable => throw new RuntimeException("A shorthand task variable is not allowed as a bare right-hand side (where no left-hand side exists) in a branch assignment")
              case _: ShorthandConfigVariable => throw new RuntimeException("A shorthand global or config variable is not allowed as a bare right-hand side (where no left-hand side exists) in a branch assignment")
              case _: ShorthandBranchGraft => throw new RuntimeException("A shorthand branch graft is not allowed as a bare right-hand side (where no left-hand side exists) in a branch assignment")
              // anonymous branch: do our best to infer a reasonable branch name
              case rhs: Literal => new BranchSpec(rhs.value, rhs)
              case rhs: TaskVariable => new BranchSpec(rhs.value, rhs)
              case rhs: BranchGraft => new BranchSpec(rhs.variableName, rhs)
              case rhs: BranchPointDef if (rhs.name.isDefined)
                => new BranchSpec(rhs.name.get, rhs)
              case rhs: SequentialBranchPoint if (rhs.branchPointName.isDefined)
                => new BranchSpec(rhs.branchPointName.get, rhs)
              case rhs: RValue => {
                throw new RuntimeException(
                  "Could not figure out how to extract a branch name from anonymous branch value %s of type %s.".
                  format(rhs.toString, rhs.getClass.getName.toString) +
                  "Please prefix with 'var=...'")
              }
            }

            if ("""[^"')(\]\[\*\$:@=\s]+""".r.pattern.matcher(spec.name).matches) {
              spec
            } else {
              throw new RuntimeException("The branch name extracted from anonymous branch with value " + spec + " does not conform to branch naming requirements")
            }
          }
      )
  )

  /** Input variable declaration. */
  val inputAssignment = basicAssignment("input",inputSpecGenerator,failure(_),err(_),err(_))

  type SpecGenerator = (String,RValue) => Spec
  type ParamSpecGenerator = (String,RValue,Boolean) => Spec
  private def inputSpecGenerator[A <: RValue](variableName:String, rval:A) = new TaskInputSpec(variableName, rval)
  def configParamSpecGenerator[A <: RValue](variableName:String, rval:A, dotVariable:Boolean) = new ConfigParamSpec(variableName, rval, dotVariable)
  private def taskParamSpecGenerator[A <: RValue](variableName:String, rval:A, dotVariable:Boolean) = new TaskParamSpec(variableName, rval, dotVariable)
  private def branchSpecGenerator[A <: RValue](variableName:String, rval:A) = new BranchSpec(variableName, rval)

  /** Output variable declaration. */
  val outputAssignment: Parser[Spec] = positioned(
      (
        name("output variable","""[=\s]|\z""".r,failure(_),err(_)) ~
        opt("=" ~> (rvalue | err("Error in output variable assignment")))
      ) ^^ {
        case (variableName: String) ~ Some(rhs: ShorthandTaskVariable) => new TaskOutputSpec(variableName,new TaskVariable(rhs.taskName,variableName))
        case (variableName: String) ~ Some(rhs: ShorthandConfigVariable) => new TaskOutputSpec(variableName,new ConfigVariable(variableName))
        case (variableName: String) ~ Some(rhs: ShorthandBranchGraft) => new TaskOutputSpec(variableName, new BranchGraft(variableName,Some(rhs.taskName),rhs.branchGraftElements))

        case (variableName: String) ~ Some(rhs: RValue) => new TaskOutputSpec(variableName,rhs)
        case (variableName: String) ~ None             => new TaskOutputSpec(variableName,Unbound())
      }
  )

  /** Output variable declaration. */
  val packageNameAssignment: Parser[Spec] = positioned({
      name("package","""\s""".r,failure(_),err(_))
    } ^^ {
    case (packageName: String) => new PackageSpec(packageName)
  })

  /** Parameter variable declaration. */
  def paramAssignment(specGenerator: ParamSpecGenerator) : Parser[Spec] = positioned(
      (   // optional dot
          opt(literal(".")) ~
          (
              name("parameter variable","""[=\s]|\z""".r,failure(_),err(_)) <~
              (
                  opt(space) ~
                  "=" ~
                  opt(space)
              )
          ) ~
          (
              rvalue | err("Error in parameter variable assignment")
          )
      ) ^^ {
        case Some(_: String) ~ (variableName: String) ~ (rhs: ShorthandTaskVariable) => specGenerator(variableName,new TaskVariable(rhs.taskName,variableName),true)
        case None            ~ (variableName: String) ~ (rhs: ShorthandTaskVariable) => specGenerator(variableName,new TaskVariable(rhs.taskName,variableName),false)

        case Some(_: String) ~ (variableName: String) ~ (rhs: ShorthandConfigVariable) => specGenerator(variableName,new ConfigVariable(variableName),true)
        case None            ~ (variableName: String) ~ (rhs: ShorthandConfigVariable) => specGenerator(variableName,new ConfigVariable(variableName),false)

        case Some(_: String) ~ (variableName: String) ~ (rhs: ShorthandBranchGraft) => specGenerator(variableName,new BranchGraft(variableName,Some(rhs.taskName),rhs.branchGraftElements),true)
        case None            ~ (variableName: String) ~ (rhs: ShorthandBranchGraft) => specGenerator(variableName,new BranchGraft(variableName,Some(rhs.taskName),rhs.branchGraftElements),false)


        case Some(_: String) ~ (variableName: String) ~ (rhs: RValue) => specGenerator(variableName,rhs,true)
        case None           ~ (variableName: String) ~ (rhs: RValue) => specGenerator(variableName,rhs,false)
      }
  )

  val configLine: Parser[ConfigAssignment] = {
    comments ~
    (
        opt(space) ~>
        paramAssignment(configParamSpecGenerator) <~
        opt(space)
    ) ~
    (comment | eol)
  } ^^ {
    case (comments: Comments) ~ (spec: Spec) ~ (_: String) => new ConfigAssignment(spec,comments)
  }

  val configLines: Parser[Seq[ConfigAssignment]] = {
    opt(whitespace) ~>
    repsep(configLine,opt(whitespace)) <~
    opt(whitespace)
  }

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
   * Sequence of <code>assignment</code>s representing output files.
   * This sequence must be preceded by ">".
   *
   */
  val taskPackageNames: Parser[TaskPackageNames] = {
    ( // Comments describe the output block
      //   There may not be any comments,
      //   in which case the comments object
      //   will contain an empty string
        ( comments<~
          // there may be whitespace after the comments
          opt(whitespace)~
          // then there must be a > character
          literal(":") ~
          // then one or more spaces or tabs
          space
         ) ~
         // Finally the list of package names
         repsep(packageNameAssignment,space)
    ) | failure("Failed to parse task package names")
  } ^^ {
    case (comments: Comments)~(list: List[Spec]) => new TaskPackageNames(list,comments)
    case _ => new TaskPackageNames(List.empty,new Comments(None))
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
         repsep(paramAssignment(taskParamSpecGenerator), space)
    ) | failure("Failed to parse task parameters")
  } ^^ {
    case comments~list => new TaskParams(list,comments)
  }


  val taskSpec: Parser[Specs] = {
    taskInputs | taskOutputs | taskParams | taskPackageNames
  }

  val convertNameToInputSpec: PartialFunction[String,Spec] = {
    case name:String => new TaskInputSpec(name,Unbound())
  }

  val convertNameToOutputSpec: PartialFunction[String,Spec] = {
    case name:String => new TaskOutputSpec(name,Unbound())
  }

  val convertNameToParamSpec: PartialFunction[String,Spec] = {
    case name:String => new TaskParamSpec(name,Unbound(), name.startsWith("."))
  }

  val convertNameToPackageSpec: PartialFunction[String,Spec] = {
    case name:String => new PackageSpec(name)
  }

  val funcSpec: Parser[Specs] = {
    ( // Comments describe the parameter block
      //   There may not be any comments,
      //   in which case the comments object
      //   will contain an empty string
      ( comments ~
        (
          // there may be whitespace after the comments
          opt(whitespace) ~>
          (
              literal("::") |
              literal("<")  |
              literal(">")  |
              literal(":")
          ) <~
          space
        )
      ) ~
      // Finally, the list of variables
      repsep(name("func variable","""\s""".r,failure(_),err(_)),space)
    )
  } ^^ {
    case (comments: Comments) ~ ("::") ~ (list: List[String]) =>
      new TaskParams(list.collect(convertNameToParamSpec),comments)
    case (comments: Comments) ~ ("<") ~ (list: List[String]) =>
      new TaskInputs(list.collect(convertNameToInputSpec),comments)
    case (comments: Comments) ~ (">") ~ (list: List[String]) =>
      new TaskOutputs(list.collect(convertNameToOutputSpec),comments)
    case (comments: Comments) ~ (":") ~ (list: List[String]) =>
      new TaskPackageNames(list.collect(convertNameToPackageSpec),comments)
  }

  val funcHeader: Parser[TaskHeader] = {
    repsep(funcSpec,regex("""[ \n\r\t]+""".r)) <~ commentableWhitespace
  } ^^ {
    case (specs: List[Specs]) => new TaskHeader(specs)
  }

  val taskHeader: Parser[TaskHeader] = {
    repsep(taskSpec,regex("""[ \n\r\t]+""".r)) <~ commentableWhitespace
  } ^^ {
    case (specs: List[Specs]) => new TaskHeader(specs)
  }

  // NOTE: This has been replaced by the more advanced bash parser
  val shellCommands: Parser[String] = {
    regex("""[^{}]*""".r) ~
    opt(
        (literal("}")~failure("Unmatched closing } bracket in bash code.")) |
        literal("{") ~
        shellCommands ~
        (literal("}")|failure("Missing closing } bracket in bash code.")) ~
        opt(shellCommands)
        //regex("""[^{}]*""".r)
    )
  } ^^ {
    case (before: String) ~ None => before
    case (before: String) ~ Some(open~block~close~None) => before + open + block + close
    case (before: String) ~ Some(open~block~close~Some(after)) => before + open + block + close + after
  }

  def taskLikeBlock[A <: TaskLike](keyword: Parser[String], blockType:String, blockGenerator: ((Comments,String,TaskHeader,BashCode) => A), header: Parser[TaskHeader]) = positioned({
    opt(whitespace) ~>
    comments ~
    (
        keyword ~>
        name
    ) ~
    (
        (whitespace | failure("Expected whitespace while parsing task-like block header, but didn't find it")) ~>
        header
    ) ~
    (
        (
            opt(whitespace) ~
            (
                literal("{") |
                failure("Missing opening { brace for " +blockType+" block.")
            )
        ) ~>
        BashGrammar.bashBlock <~
        (
            (
                literal("}") ~
                (
                    opt(space) ~
                    (eol | err("Non-whitespace character found following the " +blockType+" block closing } brace."))
                ) |
                err("Missing closing } brace for " +blockType+" block.")
            )
        )
    )
  } ^^ {
    case (comments: Comments) ~ (name: String) ~ (header: TaskHeader) ~ (commands: BashCode) =>
      blockGenerator(comments, name, header, commands)
  })

  private def actionBlockGenerator(  comments:Comments,name:String,header:TaskHeader,commands:BashCode) = new ActionDef(       comments, new Namespace(name), header, commands)
  private def baselineBlockGenerator(comments:Comments,name:String,header:TaskHeader,commands:BashCode) = new BaselineBlockDef(comments, new Namespace(name), header, commands)
  private def branchBlockGenerator(  comments:Comments,name:String,header:TaskHeader,commands:BashCode) = new BranchBlockDef(  comments, new Namespace(name), header, commands)
  private def funcBlockGenerator(    comments:Comments,name:String,header:TaskHeader,commands:BashCode) = new FuncDef(         comments, new Namespace(name), header, commands)
  private def packageBlockGenerator( comments:Comments,name:String,header:TaskHeader,commands:BashCode) = new PackageDef(      comments, new Namespace(name), header, commands)
  private def summaryOfGenerator(    comments:Comments,name:String,header:TaskHeader,commands:BashCode) = new SummaryOfDef(    comments, new Namespace(name), header, commands)
  private def taskBlockGenerator(    comments:Comments,name:String,header:TaskHeader,commands:BashCode) = new TaskDef(         comments, new Namespace(name), header, commands)

  val actionBlock:   Parser[ActionDef]        = taskLikeBlock(Keyword.action,         "action",   actionBlockGenerator,   funcHeader)
  val baselineBlock: Parser[BaselineBlockDef] = taskLikeBlock(Keyword.baseline,       "baseline", baselineBlockGenerator, taskHeader)
  val branchBlock:   Parser[BranchBlockDef]   = taskLikeBlock(Keyword.branch,         "branch",   branchBlockGenerator,   taskHeader)
  val funcBlock:     Parser[FuncDef]          = taskLikeBlock(Keyword.func,           "func",     funcBlockGenerator,     funcHeader)
  val packageBlock:  Parser[PackageDef]       = taskLikeBlock(Keyword.packageKeyword, "package",  packageBlockGenerator,  taskHeader)
  val ofBlock:       Parser[SummaryOfDef]     = taskLikeBlock(Keyword.of,             "summary",  summaryOfGenerator,     taskHeader)
  val taskBlock:     Parser[TaskDef]          = taskLikeBlock(Keyword.task,           "task",     taskBlockGenerator,     taskHeader)

  val callBlock: Parser[CallDefinition] = positioned({
    opt(whitespace) ~>
    comments ~
    (
        Keyword.task ~>
        name <~
        (
            space ~
            Keyword.calls
        )
    ) ~ name ~
    (
        (whitespace | failure("Expected whitespace while parsing call block, but didn't find it")) ~>
        taskHeader
    )
  } ^^ {
    case (comments: Comments) ~ (name: String) ~ (functionName: String) ~ (header: TaskHeader) =>
      new CallDefinition(comments,name,header,new Namespace(functionName))
  })

  def groupLikeBlock[A <: GroupLike](keyword: Parser[String], blockType: String, blockGenerator: ((Comments,String,TaskHeader,Seq[Block]) => A), header: Parser[TaskHeader], childBlock: Parser[Block]): Parser[A] = positioned({
    opt(whitespace) ~>
    comments ~
    (
        keyword ~>
        name
    ) ~
    (
        (whitespace | failure("Expected whitespace while parsing group-like block, but didn't find it")) ~>
        header
    ) ~
    (
        (
            opt(whitespace) ~
            (
                literal("{") |
                err("Missing opening { brace for " +blockType+" block.")
            ) ~
            opt(space) ~
            (
                eol |
                err("Child blocks may not start on the same line as the opening { brace.")
            )
        ) ~>
        rep(childBlock) <~
        (
            opt(whitespace) ~
            (
                literal("}") ~
                (
                    opt(space) ~
                    (eol | err("Non-whitespace character found following closing } brace for " +blockType+" block."))
                ) |
                err("Missing closing } brace for " +blockType+" block.")// If you have a closing brace but still got error message anyway, it probably means that you have have whitespace preceding your closing } brace. The closing } brace must be the first character of the line - it may not be preceded by any whitespace.")
            )
        )
    )
  } ^^ {
    case (comments: Comments) ~ (name: String) ~ (header: TaskHeader) ~ (blocks: Seq[Block]) =>
      blockGenerator(comments, name, header, blocks)
  })

  private def branchPointBlockGenerator( comments:Comments, name:String, header:TaskHeader, blocks:Seq[Block]) = new BranchPointBlock(comments, new Namespace(name), header, blocks)
  private def groupBlockGenerator(       comments:Comments, name:String, header:TaskHeader, blocks:Seq[Block]) = new GroupDef(        comments, new Namespace(name), header, blocks)
  private def submitterBlockGenerator(   comments:Comments, name:String, header:TaskHeader, blocks:Seq[Block]) = new SubmitterDef(    comments, new Namespace(name), header, blocks)
  private def summaryBlockGenerator(     comments:Comments, name:String, header:TaskHeader, blocks:Seq[Block]) = new SummaryDef(      comments, new Namespace(name), header, blocks)
  private def versionerBlockGenerator(   comments:Comments, name:String, header:TaskHeader, blocks:Seq[Block]) = new VersionerDef(    comments, new Namespace(name), header, blocks)

  def branchpointBlock: Parser[BranchPointBlock] = groupLikeBlock(Keyword.branchpoint, "branchpoint", branchPointBlockGenerator, taskHeader, branchpointChildBlock)
  def groupBlock:       Parser[GroupDef]         = groupLikeBlock(Keyword.group,       "group",       groupBlockGenerator,       taskHeader, childBlock)
  def submitterBlock:   Parser[SubmitterDef]     = groupLikeBlock(Keyword.submitter,   "submitter",   submitterBlockGenerator,   funcHeader, actionBlock)
  def summaryBlock:     Parser[SummaryDef]       = groupLikeBlock(Keyword.summmary,    "summary",     summaryBlockGenerator,     taskHeader, ofBlock)
  def versionerBlock:   Parser[VersionerDef]     = groupLikeBlock(Keyword.versioner,   "versioner",   versionerBlockGenerator,   funcHeader, actionBlock)

  val planBlock: Parser[PlanDefinition] = positioned({
    opt(whitespace) ~>
    comments ~
    (
        Keyword.plan ~>
        opt(name("plan","""\s""".r,failure(_),err(_)) <~ space)
    ) ~
    (
        (
            opt(whitespace) ~
            (
                literal("{") |
                err("Missing opening { brace for plan block.")
            ) ~
            opt(space) ~
            (
                eol |
                err("Plan lines may not start on the same line as the opening { brace.")
            )
        ) ~>
        repsep(crossProduct,opt(whitespace)) <~
        (
            opt(commentableWhitespace) ~
            (
                literal("}") ~
                (
                    opt(space) ~
                    (eol | err("Non-whitespace character found following closing } brace for plan block."))
                ) |
                err("Missing closing } brace for plan block.")
            )
        )
    )
  } ^^ {
    case (comments: Comments) ~ (name: Option[String]) ~ (lines: Seq[CrossProduct]) =>
      new PlanDefinition(comments, name, lines)
  })

  def configLikeBlock(keyword: Parser[String], blockType: String): Parser[ConfigDefinition] = positioned({
    opt(whitespace) ~>
    comments ~
    (
        keyword ~>
        opt(name(blockType, """\s""".r, failure(_), err(_)) <~ space)
    ) ~
    (
        (
            opt(whitespace) ~
            (
                literal("{") |
                err("Missing opening { brace for "+blockType+" variable block.")
            ) ~
            opt(space) ~
            (
                eol |
                err("Lines in a "+blockType+" block may not start on the same line as the opening { brace.")
            )
        ) ~>
        configLines <~
        (
            opt(whitespace) ~
            opt(comments) ~
            opt(whitespace) ~
            (
                literal("}") ~
                (
                    opt(space) ~
                    (eol | err("Non-whitespace character found following closing } brace for "+blockType+" variable block."))
                ) |
                err("Missing closing } brace for "+blockType+" variable block.")
            )
        )
    )
  } ^^ {
    case (comments: Comments) ~ (name: Option[String]) ~ (lines:Seq[ConfigAssignment]) =>
      new ConfigDefinition(blockType,comments,name,lines)
  })

  val configBlock = configLikeBlock(Keyword.config,"config")
  val globalBlock = configLikeBlock(Keyword.global,"global")

  val childBlock: Parser[Block] = {
    taskBlock | callBlock | funcBlock | summaryBlock | branchpointBlock | groupBlock
  }

  val branchpointChildBlock: Parser[Block] = {
    baselineBlock | branchBlock
  }

  val block: Parser[Block] = {
    childBlock            |
    ofBlock               |
    actionBlock           |
    submitterBlock        |
    versionerBlock        |
    packageBlock          |
    branchpointChildBlock |
    configBlock           |
    globalBlock           |
    planBlock
  }

  def importStatement(importDir: File): Parser[WorkflowDefinition] = {
    opt(whitespace) ~
    opt(comments) ~
    opt(whitespace) ~
    Keyword.importKeyword ~ opt(space) ~>
    literalValue <~
    commentableWhitespace
  }  ^^ {
    // read import statements with regard to the directory the current file is in.
    case (l:Literal) => {
      val filename: String = l.value
      val file: File = if (Files.isAbsolute(filename)) new File(filename)
                       else new File(importDir, filename)
      ErrorUtils.ex2err(GrammarParser.readWorkflow(file, isImported=true))
    }
  }

  def elements(importDir: File): Parser[Seq[ASTType]] = {
    opt(whitespace) ~>
    rep(block|importStatement(importDir)) <~
    (
        opt(whitespace)~
        opt(comments)~
        opt(whitespace)
    )
  } ^^ {
    case (e:Seq[ASTType]) => e // note: GrammarParser takes care of collapsing imports now
  }
}
