package ducttape.syntax

import org.scalatest.WordSpec
import scala.util.parsing.combinator.Parsers

import ducttape.syntax.GrammarParser.ParseResult
import ducttape.syntax.GrammarParser.Parser

abstract class NamesTest(val description:String, val parser:Parser[Any]) extends WordSpec {

  val successCases = Set(
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_"
  ) 
  
  val failureCases = Set(
    " "
  ) 
  
  val errorCases = Set(
    "A-variable_Name__"
  )
  
  
  
  "Parsing "+description should {
    
    for (value <- successCases) {   
      "succeed for \""+value+"\"" in {
        val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
        // XXX
        //Tests.verify(this,result)
      }
    }

    for (value <- failureCases) {   
      "fail for \""+value+"\"" in {
        val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
        //Tests.verifyFailure(this,result)
      }
    }       
    
    for (value <- errorCases) {   
      "fail for \""+value+"\"" in {
        val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
        //Tests.verifyError(this,result)
      }
    }    
    
  }
  
}


class VariableNameTest extends NamesTest("variable name",new Grammar(null).variableName(allowLeadingDot=false))

class BranchPointNameTest extends NamesTest("branch point name",new Grammar(null).branchPointName)

//class TaskNameTest extends NamesTest("task name",Grammar.taskName)
