package ducttape.util

import org.scalatest.WordSpec
import scala.util.parsing.combinator.Parsers
import ducttape.syntax.GrammarParser
import ducttape.syntax.GrammarParser.ParseResult
import ducttape.syntax.GrammarParser.Parser
import ducttape.syntax.Grammar._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

abstract class AbstractTest(val description:String, val parser:Parser[Any]) extends WordSpec {

  def successCases : Set[String]
  
  def failureCases : Set[String]
  
  def errorCases : Set[String]
  
  def exceptionCases : Set[String] = Set()
  
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
      "error for \""+value+"\"" in {
//        try {
          val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
          Tests.verifyError(this,result)
//        } catch {
//         case e:Exception => ()
//        }
      }
    }    
    
        for (value <- exceptionCases) {   
      "error for \""+value+"\"" in {
        try {
          val result: ParseResult[Any] = GrammarParser.parseAll(parser, value);
          Tests.verifyError(this,result)
        } catch {
         case e:Exception => ()
        }
      }
    }  
  }
  
}
