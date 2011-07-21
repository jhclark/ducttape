package ducttape.scratch {

	import scala.util.parsing.combinator.Parsers
	import scala.util.parsing.combinator.RegexParsers
	import scala.util.parsing.input.CharArrayReader
  
  
	//class WorkflowDefinition;
	
	class CommentBlock(val comments: Seq[String]) {
		override def toString = comments.mkString("\n")
		
	}
	
	class MyParser extends RegexParsers {

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
	  def emptyLine = ("\r\n" | "\n") ~ """[ \t]*""".r ~ guard(eol)
	  
	  /** Sequence of empty lines. */
	  def emptyLines = emptyLine*
	  
	  /** Contiguous block of comments. */
//	  def comments: Parser[CommentBlock] = rep1sep(comment, eol*) <~ (eol?) ^^ {
	  def comments: Parser[CommentBlock] = rep1sep(comment, eol) ^^ {
	      case c => new CommentBlock(c)
	    }
		
	  /** A single line of comment. */
	  def comment: Parser[String] = 
	    """#[ \t]*""".r ~> commentContent <~ (emptyLines) | 
	    failure("Expected a comment line, but didn't find one.") 
			  
	  /** The content portion of a single line of comment. 
	   *  Notably, this excludes the syntactic comment marker itself. 
	   */
	  def commentContent: Parser[String] = """[^\r\n]*""".r
	  
	  
	  //def workflow: WorkflowDefinition = repsep(comments,emptyLines)
	}

	
	
	object MyParseApp extends MyParser with Application {

	  val sample = """# Welcome to make
# This is a sample

# Comments


blah blah - this line should cause a parse failure

# Another comment
""";
	    
	val result: ParseResult[CommentBlock] = parseAll(comments, sample);
	
	result match {
	  case Success(result,_) => println(result)
	  case x => println(x)
	  //case Failure(error,inputReader) => println(error)
	}

}

	
}