package ducttape.syntax

import org.scalatest.WordSpec
import scala.util.parsing.combinator.Parsers

import ducttape.syntax.GrammarParser.ParseResult
import ducttape.util.Tests


class NamesTest extends WordSpec {

  val successCases = Set(
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_"
  ) 
  
  val errorCases = Set(
    "A-variable_Name__",
    " "
  ) 
  
  
  "Parsing variable name" should {
    
    for (value <- successCases) {   
      "succeed for \""+value+"\"" in {
        val result: ParseResult[String] = GrammarParser.parseAll(Grammar.variableName, value);
        Tests.verify(this,result)
      }
    }
    
    for (value <- errorCases) {   
      "fail for \""+value+"\"" in {
        val result: ParseResult[String] = GrammarParser.parseAll(Grammar.variableName, value);
        Tests.verifyError(this,result)
      }
    }    
    
  }
  
}