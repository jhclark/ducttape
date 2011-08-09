package ducttape.syntax 

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader

class FileFormatException(msg: String) extends Exception(msg) {}

object GrammarParser extends RegexParsers {
  
  import ducttape.syntax.AbstractSyntaxTree._
  import ducttape.syntax.Grammar._
  import java.io.Reader
	
  override val skipWhitespace = false
  // use IO.reader to get a reader (force files to be UTF-8 encoded?)
  def read(reader: Reader): WorkflowDefinition = {      
    val result: ParseResult[WorkflowDefinition] = parseAll(workflow, reader)
    val pos = result.next.pos
    return result match {
      case Success(res, _) => res
      case Failure(msg, _) =>
  	throw new FileFormatException("ERROR: line %d column %d: %s".format(pos.line, pos.column, msg))
      case Error(msg, _) =>
  	throw new FileFormatException("HARD ERROR: line %d column %d: %s".format(pos.line, pos.column, msg))
    }
  }
}

/**
 * Grammar for ducttape scripts
 * 
 * @author Jon Clark
 * @author Lane Schwartz
 */
object Grammar {

  import GrammarParser._
  import ducttape.syntax.AbstractSyntaxTree._

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

  /** Contiguous block of comments. */
  val comments: Parser[CommentBlock] = positioned(opt(repsep(comment, eol))<~(rep(emptyLine)~opt(eol)) ^^ {
    case Some(c) => new CommentBlock(c)
    case None => new CommentBlock(Seq())
  })

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
   */
  def variableName: Parser[String] = {
    // If the name starts with an illegal character, bail out and don't backtrack
    ( ("""[^A-Za-z_]""".r <~ failure("Illegal character at start of variable name"))

     // Else if the name starts out OK, but then contains an illegal non-whitespace character, bail out and don't backtrack
     | ("""[A-Za-z_][A-Za-z0-9_]*""".r <~ guard(regex("""[^\r\n= \t]+""".r))
     ~! err("Illegal character in variable name declaration"))
    
     // Finally, if the name contains only legal characters, then parse it!
     | ("""[A-Za-z_][A-Za-z0-9_]*""".r) //| err("")
   )
  }

  /** Name of a branch point.
   *  <p>
   *  The name must conform to Bash variable name requirements: 
   *  "A word consisting solely of letters, numbers, and underscores, and beginning with a letter or underscore."
   */
  def branchPointName: Parser[String] = {
    // If the name starts with an illegal character, bail out and don't backtrack
    ( """[^A-Za-z_]""".r<~failure("Illegal character at start of branch point name")

     // Else if the name starts out OK, but then contains an illegal non-whitespace character, bail out and don't backtrack
     | """[A-Za-z_][A-Za-z0-9_]*""".r <~ guard(regex("""[^\r\n: \t]+""".r))
     ~! err("Illegal character in branch point name declaration")
    
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
    "[" ~> (
    // If the name starts with an illegal character, bail out and don't backtrack
    """[^A-Za-z_]""".r<~err("Illegal character at start of task name")
	
    // Else if the name starts out OK, but then contains an illegal non-whitespace character, bail out and don't backtrack
    | """[A-Za-z_][A-Za-z0-9_]*""".r<~guard(regex("""[^\r\n\] \t]+""".r))~!err("Illegal character in task name")

    // Finally, if the name contains only legal characters, then parse it!
    | """[A-Za-z_][A-Za-z0-9_]*""".r // | failure("")
    ) <~ "]"
  }
			
  /** A task name, preceded by a dollar sign and followed by a slash.
   *  The task name (but not the dollar sign or slash) may be optionally enclosed in curly brackets.
   *  <p>
   *  The name must conform to Bash variable name requirements: 
   *  "A word consisting solely of letters, numbers, and underscores, and beginning with a letter or underscore."
   */
  def taskRef: Parser[String] = {
    literal("$") ~> (
      ( literal("{") ~> (
	// If the name starts with an illegal character, bail out and don't backtrack
	"""[^A-Za-z_-]""".r <~ (success() ~! failure("Illegal character at start of task name"))

	// Else if the name starts out OK, but then contains an illegal non-whitespace character, bail out and don't backtrack
	| """[A-Za-z_-][A-Za-z0-9_-]*""".r <~ guard(regex("""[^\r\n} \t]+""".r))
        ~!failure("Illegal character in task name")

	// Finally, if the name contains only legal characters, then parse it!
	| """[A-Za-z_-][A-Za-z0-9_-]*""".r) <~ literal("}")
     ) | (

	// If the name starts with an illegal character, bail out and don't backtrack
	"""[^A-Za-z_-]""".r <~ (success() ~! failure("Illegal character at start of task name"))

	// Else if the name starts out OK, but then contains an illegal non-whitespace character, bail out and don't backtrack
	| """[A-Za-z_-][A-Za-z0-9_-]*""".r <~ guard(regex("""[^\r\n/ \t]+""".r))
        ~! failure("Illegal character in task name")

	// Finally, if the name contains only legal characters, then parse it!
	| """[A-Za-z_-][A-Za-z0-9_-]*""".r
      )
    )
  }
		
  /** A <code>taskRef</code>, followed by a variable name.
   *  <p>
   *  The name must conform to Bash variable name requirements: 
   *  "A word consisting solely of letters, numbers, and underscores, and beginning with a letter or underscore."
   */
  def variableRef: Parser[Variable] = { positioned(
    taskRef ~ literal("/") ~
    (
    
      // If the name starts with an illegal character, bail out and don't backtrack
      """[^A-Za-z_-]""".r <~ failure("Illegal character at start of variable name")

      // Else if the name starts out OK, but then contains an illegal non-whitespace character, bail out and don't backtrack
      | """[A-Za-z_-][A-Za-z0-9_-]*""".r <~ guard(regex("""[^\r\n \t]+""".r))
      ~! failure("Illegal character in variable name")

      // Finally, if the name contains only legal characters, then parse it!
      | """[A-Za-z_-][A-Za-z0-9_-]*""".r
    ) ^^ {
      case taskRefString~slash~nonSpaceString => new Variable(taskRefString,nonSpaceString)
    }
  )}
	  								
    /** 
     * String sequence that does not begin with a dollar sign, and ends at the first whitespace character.
     */
    def rvalueLiteral: Parser[Literal] = (
      // If we find a value followed immediately by a colon, bail out and don't backtrack
      regex("""[^\r\n$ \t]+""".r) <~ guard(":".r)
      ~! err("Right hand side value contains prohibited character `:'")
      
      // Finally, if the name contains only legal characters, then parse it!
      | regex("""[^\r\n$ \t]+""".r)
      ) ^^ {
	case strValue: String => new Literal(strValue)
      }
	
    /** Branch declaration */
    def branchPoint: Parser[BranchPointDef] = positioned(
      (((literal("(") ~! ((space) | err("Looks like you forgot to leave a space after your opening parenthesis. Yeah, we know that's a pain - sorry."))) ~> branchPointName <~ literal(":")) ~  
       (rep(space) ~> repsep(assignment, space)) <~ ((space ~ literal(")") | err("Looks like you forgot to leave a space before your closing parenthesis. Yeah, we know that's a pain - sorry.")))) ^^ {
         case strVar ~ seq => new BranchPointDef(strVar,seq)
       })

    /** Right hand side of a variable declaration. */
    def rvalue: Parser[RValue] = positioned((variableRef | branchPoint | rvalueLiteral) ^^ {
      case varRef: Variable => varRef;
      case branchPoint: BranchPointDef => branchPoint;
      case lit: Literal => lit;
    })

    /** Variable declaration */
    def assignment: Parser[Spec] = positioned(variableName ~ opt("=" ~! rvalue) ^^ {
      case strVar ~ Some(e ~ value) => new Spec(strVar, value)
      case strVar ~ None => new Spec(strVar, Unbound())
    })

    // === TASKS ===
    
    /** Task header line, consisting of 
     * <code>taskName</code>, <code>taskInputs</code>, <code>taskOutputs</code>, and <code>taskParams</code>. 
     */
    def taskHeader: Parser[TaskHeader]
      = positioned(taskName ~ (space*) ~ taskInputs ~ (space*) ~ taskOutputs ~ (space*) ~ taskParams <~ eol ^^ {
        case name ~ sp1 ~ targets ~ sp2 ~ deps ~ sp3 ~ params => new TaskHeader(name, targets, deps, params)
      })

    /**
     * Sequence of <code>assignment</code>s representing input files.
     * This sequence must be preceded by "<".
     */
    def taskInputs: Parser[Seq[Spec]] = opt("<" ~ rep(space) ~> repsep(assignment, space)) ^^ {
      case Some(list) => list
      case None => List.empty
    } 

    /**
     * Sequence of <code>assignment</code>s representing input files.
     * This sequence must be preceded by ">".
     */
    def taskOutputs: Parser[Seq[Spec]] = opt(">" ~ rep(space) ~> repsep(assignment, space)) ^^ {
      case Some(list) => list
      case None => List.empty
    }

    /**
     * Sequence of <code>assignment</code>s representing input files.
     * This sequence must be preceded by "::".
     */	
    def taskParams: Parser[Seq[Spec]] = opt("::" ~ rep(space) ~> repsep(assignment, space)) ^^ {
        case Some(params) => params
        case None => List.empty
      }
    
    /** Shell command. */
    def command: Parser[String] =
      // It would be nice to have operator that acted like <~ composed with ~! 
      // I use <~success()~! to get the result of non-back-tracking sequential composition
      // which only keeps the right result
      ( guard(nonSpace) <~ success() ~! failure("Command must be preceded by whitespace")
      // If a command is preceded by whitespace, parse it!
      | (rep1(space) ~> """[^\r\n]+""".r)
      )
    
    /** Sequence of commands, separated by end-of-line character(s). */
    def commands: Parser[Seq[String]] = repsep(command, eol) <~ (eol?)
    
    /** Complete declaration of a task, including command(s) and optional comments. */
    def taskBlock: Parser[TaskDef] =  positioned(comments ~ taskHeader ~! commands <~ emptyLines ^^ {
    	case ((com:CommentBlock) ~ (head:TaskHeader) ~ (cmds:Seq[String])) => 
    	  new TaskDef(head.name, com, head.inputs, head.outputs, head.params, cmds)
    })
    
    /** Complete declaration of a hyperworkflow of tasks. */
    def workflow: Parser[WorkflowDefinition] = positioned(repsep(taskBlock,eol) ^^ {
    	case w => new WorkflowDefinition(w)
    })
}
