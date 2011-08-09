package ducttape.syntax

import org.scalatest.WordSpec
import scala.util.parsing.combinator.Parsers

import ducttape.syntax.GrammarParser.ParseResult
import ducttape.syntax.GrammarParser.Parser
import ducttape.util.Tests

class TaskTest extends WordSpec {

  val successCases = Map(

"a bare task" ->
"""[hello_world]
""",

"a task with unbound outputs" ->
"""[hello_world_2] > x y_txt
""",

"a task with bound inputs" ->
"""[hello_world_3] < a=/etc/passwd b=/etc/hosts
""",

"a task with a bound dependent input and an unbound output" ->
"""[and_then] < a=$first/x > x
""",

"a task with a bound input, an unbound output, and a bound parameter" ->
"""[param_step] < in=/etc/passwd > out :: N=5
""",
      
"a task with bound branch input and unbound output" ->
"""[has_branches] < in=( whichSize: smaller=small.txt bigger=big.txt ) > out
"""

  )
  
   val errorCases = Map(
    		"an invalid task declaration" -> 
        """ blah blah blah! """
    )
    
    
    
    "Parsing" should {
    
    for ((key,value) <- successCases) {   
      "succeed for "+key in {
        val result: ParseResult[Any] = GrammarParser.parseAll(Grammar.taskHeader, value);
        Tests.verify(this,result)
      }
    }
    
    for ((key,value) <- errorCases) {   
      "fail for "+key in {
        val result: ParseResult[Any] = GrammarParser.parseAll(Grammar.taskHeader, value);
        Tests.verifyFailure(this,result)
      }
    }    
    
  }  
  
}