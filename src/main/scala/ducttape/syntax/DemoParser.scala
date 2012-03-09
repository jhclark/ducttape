package ducttape.syntax
//import scala.util.parsing.combinator.Parsers.ParseResult
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.Grammar._
import ducttape.syntax.GrammarParser._
  
object DemoParser extends Application {

  {
    val result: ParseResult[Tape] = parseAll(Grammar.tape,"""[hello]""")   
    println(result.get)
  }
  
  {
    val result: ParseResult[Literal] = parseAll(Grammar.quotedLiteral,"""'hi\tthere\nc:\\\r\nworld'""")   
    println(result.get)    
  }

  {
    val result: ParseResult[VariableReference] = parseAll(Grammar.variableReference,"""$abc""")   
    println(result.get)
  }

}