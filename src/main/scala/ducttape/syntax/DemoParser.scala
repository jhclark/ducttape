package ducttape.syntax
//import scala.util.parsing.combinator.Parsers.ParseResult
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.Grammar._
import ducttape.syntax.GrammarParser._
  
object DemoParser extends Application {

  def print(r:ParseResult[_]) = {
    r match {
      case success:Success[_] => println(success.get)
      case failure:Failure          => println(failure.msg)
      case error:Error              => println(error.msg)
    }
  }

  
  {
    val result: ParseResult[Tape] = parseAll(Grammar.tape,"""[hello]""")   
    print(result)
  }
  
  {
    val result: ParseResult[Literal] = parseAll(Grammar.quotedLiteral,"""'hi\tthere\nc:\\\r\nworld'""")   
    print(result)    
  }

  {
    val result: ParseResult[Variable] = parseAll(Grammar.variableReference,"""$abc""")   
    print(result)
  }

  {
    val result: ParseResult[Number] = parseAll(Grammar.number,"""-.5""")   
    print(result)
  }  
  
}