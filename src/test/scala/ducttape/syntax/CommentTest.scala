package ducttape.syntax 

import org.scalatest.WordSpec
import scala.util.parsing.combinator.Parsers

import ducttape.syntax.GrammarParser.ParseResult
import ducttape.syntax.GrammarParser.Parser
import ducttape.util.Tests


class CommentTest extends WordSpec {

  val successCases = Map(

"a single-line comment" -> 
"""# Welcome to make
""",


"a single-line comment with preceding white space" ->
"""	 	# Welcome to make
""",


"a single-line comment with no trailing newline" ->
"""# Welcome to make""",


"a multi-line comment" ->
"""# Welcome to make
# This is a sample
# Comments
# blah blah - this line should cause a parse failure
# Another comment
""",


"a multi-line comment with preceding white space" ->
""" # Welcome to make
			   # This is a sample
  # Comments
 # blah blah - this line should cause a parse failure
			  # Another comment
""",

"a multi-line comment with interspersed blank lines" ->
"""# Welcome to make
# This is a sample

# Comments


# blah blah - this line should cause a parse failure

# Another comment
""",

"a multi-line comment with preceding white space and interspersed blank lines" ->
"""# Welcome to make
				# This is a sample

				# Comments


				# blah blah - this line should cause a parse failure

				# Another comment
""",

"a multi-line comment with preceding white space and interspersed blank lines followed by blank lines" ->
"""# Welcome to make
				# This is a sample

				# Comments


				# blah blah - this line should cause a parse failure

				# Another comment


""",


"a multi-line comment with preceding white space and interspersed white space lines followed by blank lines" ->
"""# Welcome to make
				# This is a sample
				   
				# Comments


				# blah blah - this line should cause a parse failure
 
				# Another comment


"""
  )


    val errorCases = Map(
    		"A non-comment line" -> 
        """[jobName]"""
    )
    
    
    
    "Parsing comment" should {
    
    for ((key,value) <- successCases) {   
      "succeed for "+key in {
        val result: ParseResult[Any] = GrammarParser.parseAll(Grammar.comments, value);
        Tests.verify(this,result)
      }
    }
    
    for ((key,value) <- errorCases) {   
      "fail for "+key in {
        val result: ParseResult[Any] = GrammarParser.parseAll(Grammar.comments, value);
        Tests.verifyFailure(this,result)
      }
    }    
    
  }
  
  
  
}
