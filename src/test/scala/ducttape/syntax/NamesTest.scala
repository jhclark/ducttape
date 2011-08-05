package ducttape.syntax

import org.scalatest.WordSpec
import scala.util.parsing.combinator.Parsers

import ducttape.syntax.GrammarParser.ParseResult
import ducttape.syntax.GrammarParser.Parser
import ducttape.util.Tests


abstract class NamesTest(val description:String, val parser:Parser[Any]) extends WordSpec {

  val successCases = Set(
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_"
  ) 
  
  val errorCases = Set(
    "A-variable_Name__",
    " "
  ) 
  
  
  "Parsing "+description should {
    
    for (value <- successCases) {   
      "succeed for \""+value+"\"" in {
        val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
        Tests.verify(this,result)
      }
    }
    
    for (value <- errorCases) {   
      "fail for \""+value+"\"" in {
        val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
        Tests.verifyError(this,result)
      }
    }    
    
  }
  
}


class VariableNameTest extends NamesTest("variable name",Grammar.variableName)

class BranchPointNameTest extends NamesTest("branch point name",Grammar.branchPointName)

//class TaskNameTest extends NamesTest("task name",Grammar.taskName)
