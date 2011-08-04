package ducttape.syntax 

import org.scalatest.FlatSpec
import scala.util.parsing.combinator.Parsers

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.Grammar._
import ducttape.syntax.GrammarParser._

class CommentTest extends FlatSpec {

	def verify(result:ParseResult[Any]) : Unit = {
	  result match {
			case Success(res, _) => ()
			case Failure(msg, _) => fail(msg)
			case Error(msg, _)   => fail(msg)
	  }
	}
  
	"A single-line comment" should "parse successfully" in {
		val sampleComment = """# Welcome to make
""";

		val result: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

		verify(result)

	}
	
	"A single-line comment with preceding white space" should "parse successfully" in {
		val sampleComment = 
"""	 	# Welcome to make
""";

		val result: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

		verify(result)

	}	

	"A single-line comment with no trailing newline" should "parse successfully" in {
		val sampleComment = """# Welcome to make""";

		val result: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

		verify(result)

	}
	
	"A multi-line comment" should "parse successfully" in {

	  val sampleComment = 
"""# Welcome to make
# This is a sample
# Comments
# blah blah - this line should cause a parse failure
# Another comment
""";
		
		val result: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

		verify(result)
	}	
	
	
	
	"A multi-line comment with preceding white space" should "parse successfully" in {

	  val sampleComment = 
""" # Welcome to make
			   # This is a sample
  # Comments
 # blah blah - this line should cause a parse failure
			  # Another comment
""";
		
		val result: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

		verify(result)
	}		
	

	"A multi-line comment with interspersed blank lines" should "parse successfully" in {
		val sampleComment = 
"""# Welcome to make
# This is a sample

# Comments


# blah blah - this line should cause a parse failure

# Another comment
""";

		
		val result: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

		verify(result)
	}	
	
	
	"A multi-line comment with preceding white space and interspersed blank lines" should "parse successfully" in {
		val sampleComment = """# Welcome to make
				# This is a sample

				# Comments


				# blah blah - this line should cause a parse failure

				# Another comment
""";

		
		val result: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

		verify(result)
	}

	"A multi-line comment with preceding white space and interspersed blank lines followed by blank lines" should "parse successfully" in {
		val sampleComment = """# Welcome to make
				# This is a sample

				# Comments


				# blah blah - this line should cause a parse failure

				# Another comment


""";

		
		val result: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

		verify(result)
	}	
	
	"A multi-line comment with preceding white space and interspersed white space lines followed by blank lines" should "parse successfully" in {
		val sampleComment = """# Welcome to make
				# This is a sample
				   
				# Comments


				# blah blah - this line should cause a parse failure
 
				# Another comment


""";

		
		val result: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

		verify(result)
	}		
}


class TaskHeaderTest extends FlatSpec {


}