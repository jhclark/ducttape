package ducttape.syntax 

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader
import ducttape.syntax.AbstractSyntaxTree._

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
	def comments: Parser[CommentBlock] = positioned(rep1sep(comment, eol) ^^ {
		case c => new CommentBlock(c)
	});

	/** A single line of comment. */
	def comment: Parser[String] = 
		"""[ \t]*#[ \t]*""".r ~> commentContent <~ (emptyLines) | 
		failure("Expected a comment line, but didn't find one."); 

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
			"""[A-Za-z_-][A-Za-z0-9_-]*""".r<~guard(regex("""[^\r\n \t=]+""".r))~!failure("Illegal character in variable name") |
			// Finally, if the name contains only legal characters, then parse it!
			"""[A-Za-z_-][A-Za-z0-9_-]*""".r

	/** Name of a task, enclosed in square brackets. */
	def taskName = "[" ~> name <~ "]";

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
	def rvalue: Parser[RValue] = opt("$" ~! name ~! "/") ~! value ^^ {
		case None ~ strVal => new Literal(strVal);
		case Some("$" ~ strTask ~ slash) ~ strVal => new Variable(strTask, strVal);
	}

	/** Variable declaration */
	def assignment: Parser[Spec] = name ~ opt("=" ~! rvalue) ^^ {
		case strVar ~ Some(e ~ value) => new Spec(strVar, value)
		case strVar ~ None => new Spec(strVar, Unbound())
	} 
	
	/////////////////////////////////////////////////////////////////////////
	//                                                                     //
	//                                TASKS                                //
	//                                                                     //
	/////////////////////////////////////////////////////////////////////////	  

	def taskHeader: Parser[TaskHeader] = taskName <~ eol ^^ {
		case name => new TaskHeader(name)
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
	val taskNameResult: ParseResult[TaskHeader] = parseAll(taskHeader,sampleTaskName);

	val sampleAssignment = """< foo=a"""
	val assignmentResult: ParseResult[Seq[Spec]] = parseAll(taskInputs,sampleAssignment);
	
	val result = 
			//commentResult;
			//taskNameResult;
			assignmentResult;

	result match {
		case Success(result,_) => println(result)
		case x => println(x)
		//case Failure(error,inputReader) => println(error)
	}

}


