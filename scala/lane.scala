package ducttape.syntax 

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader


class Grammar extends RegexParsers {

	/////////////////////////////////////////////////////////////////////////
	//                                                                     //
	//                             WHITE SPACE                             //
	//                                                                     //
	/////////////////////////////////////////////////////////////////////////

	override val skipWhitespace = false;

	/** End of line characters */
	def eol = "\r\n" | "\n" | CharArrayReader.EofCh;

	/** Non-end of line white space characters */
	def space = """[ \t]+""".r;

	/**
	 * End of line, optionally followed by more whitespace, followed by another end of line. 
	 * <p>
	 * This second end of line is recognized, but not consumed.
	 */
	def emptyLine = ("\r\n" | "\n") ~ """[ \t]*""".r ~ guard(eol);

	/** Sequence of empty lines. */
	def emptyLines = emptyLine*;



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
	//                                NAMES                                //
	//                                                                     //
	/////////////////////////////////////////////////////////////////////////

	/** 
	 * "A word consisting solely of letters, numbers, and underscores,
	 * and beginning with a letter or underscore." 
	 * This definition for a name is taken directly from Bash.*/
	def name: Parser[String] = """[A-Za-z_-][A-Za-z0-9_-]*""".r;

	/** Name of a task, enclosed in square brackets. */
	def taskName: Parser[String] = "[" ~> name <~ "]";



	/////////////////////////////////////////////////////////////////////////
	//                                                                     //
	//                                TASKS                                //
	//                                                                     //
	/////////////////////////////////////////////////////////////////////////	  

	def taskHeader: Parser[TaskHeader] = taskName <~ eol ^^ {
		case name => new TaskHeader(name)
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

	val result = 
			//commentResult;
			taskNameResult;


	result match {
		case Success(result,_) => println(result)
		case x => println(x)
		//case Failure(error,inputReader) => println(error)
	}

}


