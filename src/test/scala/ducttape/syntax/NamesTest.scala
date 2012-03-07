package ducttape.syntax

import org.scalatest.WordSpec
import scala.util.parsing.combinator.Parsers
import ducttape.syntax.GrammarParser.ParseResult
import ducttape.syntax.GrammarParser.Parser
import ducttape.util.Tests
import ducttape.syntax.Grammar._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

abstract class NamesTest(val description:String, val parser:Parser[Any]) extends WordSpec {

  val successCases = Set(
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_"
  ) 
  
  val failureCases = Set(
    ""
  ) 
  
  val errorCases = Set(
    " ",
    "A-variable_Name__"
  )
  
  
  
  "Parsing "+description should {
    
    for (value <- successCases) {   
      "succeed for \""+value+"\"" in {
        val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
        Tests.verify(this,result)
      }
    }

    for (value <- failureCases) {   
      "fail for \""+value+"\"" in {
        val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
        Tests.verifyFailure(this,result)
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

//@RunWith(classOf[JUnitRunner])
//class BareNameTest extends NamesTest("variable name",Grammar.name("""\z""".r))
//
//class BranchPointNameTest extends NamesTest("branch point name",Grammar.tape)

//class TaskNameTest extends NamesTest("task name",Grammar.taskName)
