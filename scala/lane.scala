package ducttape.syntax 

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader
import ducttape.syntax.AbstractSyntaxTree._

/**
 * Grammar for ducttape scripts
 * 
 * @author Jon Clark
 * @author Lane Schwartz
 */
class Grammar extends RegexParsers {

	/////////////////////////////////////////////////////////////////////////
	//                                                                     //
	//                             WHITE SPACE                             //
	//                                                                     //
	/////////////////////////////////////////////////////////////////////////

	override val skipWhitespace = false;

	/** End of line characters */
	def eol: Parser[String] = literal("\r\n") | literal("\n") | literal(CharArrayReader.EofCh.toString);

	/** Non-end of line white space characters */
	def space: Parser[String] = regex("""[ \t]+""".r);

	/**
	 * End of line, optionally followed by more whitespace, followed by another end of line. 
	 * <p>
	 * This second end of line is recognized, but not consumed.
	 */
	def emptyLine = (literal("\r\n") | literal("\n")) ~ regex("""[ \t]*""".r) ~ guard(eol);

	/** Sequence of empty lines. */
	def emptyLines = emptyLine*;

	/** Non-white space sequence. */
	def nonSpace: Parser[String] = regex("""[^\r\n \t]+""".r)

	
	/////////////////////////////////////////////////////////////////////////
	//                                                                     //
	//                              COMMENTS                               //
	//                                                                     //
	/////////////////////////////////////////////////////////////////////////

	/** Contiguous block of comments. */
	def comments: Parser[CommentBlock] = opt(repsep(comment, eol))<~opt(eol) ^^ {
		case Some(c) => new CommentBlock(c)
		case None => new CommentBlock(Seq())
	};

	/** A single line of comment. */
	def comment: Parser[String] = 
		"""[ \n\r\t]*#[ \t]*""".r ~> commentContent <~ guard(eol) //| failure("Expected a comment line, but didn't find one."); 

	/** The content portion of a single line of comment. 
	 *  Notably, this excludes the syntactic comment marker itself. 
	 */
	def commentContent: Parser[String] = """[^\r\n]*""".r;



	/////////////////////////////////////////////////////////////////////////
	//                                                                     //
	//                        NAMES & VALUES                               //
	//                                                                     //
	/////////////////////////////////////////////////////////////////////////

	/** 
	 * "A word consisting solely of letters, numbers, and underscores,
	 * and beginning with a letter or underscore." 
	 * This definition for a name is taken directly from Bash.*/
	def name: Parser[String] = 
			// If the name starts with an illegal character, bail out and don't backtrack
			"""[^A-Za-z_-]""".r<~failure("Illegal character at start of variable name") |
			// Else if the name starts out OK, but then contains an illegal non-whitespace character, bail out and don't backtrack
			"""[A-Za-z_-][A-Za-z0-9_-]*""".r<~guard(nonSpace)~!failure("Illegal character in variable name") |
			// Finally, if the name contains only legal characters, then parse it!
			"""[A-Za-z_-][A-Za-z0-9_-]*""".r// | failure("")

	/** Name of a task, enclosed in square brackets. */
	def taskName: Parser[String] = "[" ~> (
			// If the name starts with an illegal character, bail out and don't backtrack
			"""[^A-Za-z_-]""".r<~failure("Illegal character at start of variable name") |
			// Else if the name starts out OK, but then contains an illegal non-whitespace character, bail out and don't backtrack
			"""[A-Za-z_-][A-Za-z0-9_-]*""".r<~guard(regex("""[^\r\n\] \t]+""".r))~!failure("Illegal character in task name") |
			// Finally, if the name contains only legal characters, then parse it!
			"""[A-Za-z_-][A-Za-z0-9_-]*""".r// | failure("")
			) <~ "]"
			
	/** Name of a variable, possibly followed by an equals sign. */
	def variableName: Parser[String] = 
			// If the name starts with an illegal character, bail out and don't backtrack
			"""[^A-Za-z_-]""".r<~failure("Illegal character at start of variable name") |
			// Else if the name starts out OK, but then contains an illegal non-whitespace character, bail out and don't backtrack
			"""[A-Za-z_-][A-Za-z0-9_-]*""".r<~guard(regex("""[^\r\n= \t]+""".r))~!failure("Illegal character in variable name declaration") |
			// Finally, if the name contains only legal characters, then parse it!
			"""[A-Za-z_-][A-Za-z0-9_-]*""".r// | failure("")
						
			
	/** 
	 * String sequence that does not contain prohibited characters 
	 * (end-of-line characters, space, tab, colon)
	 */
	def value = 
			// If we find a value followed immediately by a colon, bail out and don't backtrack
			regex("""[^\r\n: \t]+""".r)<~guard(":".r)~!failure("Right hand side value contains prohibited character `:'") |
			// Finally, if the name contains only legal characters, then parse it!
			regex("""[^\r\n: \t]+""".r)

	/** Right hand side of a variable declaration. */
	def rvalue: Parser[RValue] = (opt("$"~!name~!"/")) ~! value ^^ {
		case None ~ strVal => new Literal(strVal);
		case Some("$" ~ strTask ~ slash) ~ strVal => new Variable(strTask, strVal);
	}

	/** Variable declaration */
	def assignment: Parser[Spec] = variableName ~ opt("=" ~! rvalue) ^^ {
		case strVar ~ Some(e ~ value) => new Spec(strVar, value)
		case strVar ~ None => new Spec(strVar, Unbound())
	} 
	
	/////////////////////////////////////////////////////////////////////////
	//                                                                     //
	//                                TASKS                                //
	//                                                                     //
	/////////////////////////////////////////////////////////////////////////	  

	def taskHeader: Parser[TaskHeader]
			= taskName ~ (space*) ~ taskInputs ~ (space*) ~ taskOutputs ~ (space*) ~ taskParams <~ eol ^^ {
		case name ~ sp1 ~ targets ~ sp2 ~ deps ~ sp3 ~ params => new TaskHeader(name, targets, deps, params)
    }

	def taskInputs: Parser[Seq[Spec]] = opt("<" ~ rep(space) ~> repsep(assignment, space)) ^^ {
    	case Some(list) => list
    	case None => List.empty
	} 
	
	def taskOutputs: Parser[Seq[Spec]] = opt(">" ~ rep(space) ~> repsep(assignment, space)) ^^ {
		case Some(list) => list
		case None => List.empty
    }

    def taskParams: Parser[Seq[Spec]] = opt("::" ~ rep(space) ~> repsep(assignment, space)) ^^ {
    	case Some(params) => params
    	case None => List.empty
	}
    
    
    /** Shell command. */
    def command: Parser[String] =
    		// It would be nice to have operator that acted like <~ composed with ~! 
    		// I use <~success()~! to get the result of non-back-tracking sequential composition
    		//   which only keeps the right result
    		(guard(nonSpace)<~success()~!failure("Command must be preceded by whitespace")) |
    		// If a command is preceded by whitespace, parse it!
    		(rep1(space) ~> """[^\r\n]+""".r)
    
    /** Sequence of commands, separated by end-of-line character(s). */
    def commands: Parser[Seq[String]] = repsep(command, eol) <~ (eol?)
    
    /** Complete declaration of a task, including command(s) and optional comments. */
    def taskBlock: Parser[TaskDef] =  comments ~ taskHeader ~! commands <~ emptyLines ^^ {
    	case ((com:CommentBlock) ~ (head:TaskHeader) ~ (cmds:Seq[String])) => 
    	  new TaskDef(head.name, com, head.inputs, head.outputs, head.params, cmds)
    }
    
}



object MyParseApp extends Grammar with Application {

	val sampleComment = """# Welcome to make
			# This is a sample

			# Comments


			# blah blah - this line should cause a parse failure

			# Another comment
			""";

	val commentResult: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

	val sampleTaskName = """[myTask]""";
	val taskNameResult: ParseResult[Any] = parseAll(taskName,sampleTaskName);

	val sampleAssignment = """foo=a"""
	val assignmentResult: ParseResult[Spec] = parseAll(assignment,sampleAssignment);
	
	val sampleInputs = """< x1=/tmp/bar y=yow"""
	val inputsResult: ParseResult[Seq[Spec]] = parseAll(taskInputs,sampleInputs)

	val sampleOutputs = """> x1=/tmp/bar y=yow"""
	val outputResult: ParseResult[Seq[Spec]] = parseAll(taskOutputs,sampleOutputs)
	
	val sampleParams = """:: x1=/tmp/bar y=yow"""
	val paramsResult: ParseResult[Seq[Spec]] = parseAll(taskParams,sampleParams)	
	
	val sampleHeader = """[foo] < x1=/tmp/bar y=yow/foo > out=/path/to/^crrazy! :: param=3.1415
""";
	val headerResult: ParseResult[TaskHeader] = parseAll(taskHeader,sampleHeader)
	
	val sampleCommand = """ wc -l $in > $out""";
	val commandResult: ParseResult[String] = parseAll(command,sampleCommand)

	val sampleCommands = """ wc -l $in > $out
 cat < $in > $out
""";
	val commandsResult: ParseResult[Seq[String]] = parseAll(commands,sampleCommands)
	
	val sampleTaskBlock = 
"""

# Hello

# Welcome
[myTask] < input > output=/path/to/foo :: n=5
    cat < $input > $output
""";
	val taskBlockResult: ParseResult[TaskDef] = parseAll(taskBlock,sampleTaskBlock)
	
	val result = 
			//commentResult;
			//taskNameResult;
			//assignmentResult;
	  		//inputsResult;
	  		//outputResult;
	  		//paramsResult;
			//headerResult;
	  		//commandResult;
			//commandsResult;
	  		taskBlockResult;

	result match {
		case Success(result,_) => println(result)
		case x => println(x)
		//case Failure(error,inputReader) => println(error)
	}

}


