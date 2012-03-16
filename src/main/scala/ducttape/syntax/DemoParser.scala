package ducttape.syntax
//import scala.util.parsing.combinator.Parsers.ParseResult
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.Grammar._
import ducttape.syntax.GrammarParser._
  
object DemoParser extends Application {

  def print(r:ParseResult[_]) = {
    r match {
      case success:Success[_] => println(success.get)
      case failure:Failure    => println(failure.msg)
      case error:Error        => println(error.msg)
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
  
  {
    val result: ParseResult[Literal] = parseAll(Grammar.tripleQuotedLiteral,"\"\"\"" + "This has line breaks in\nit!" + "\"\"\"")   
    print(result)
  }  
  
  {
    val result: ParseResult[BranchPointDef] = parseAll(Grammar.branchPoint,"(a: (b: c=(x: x1=$d@taskE x2=farOut x3=\"\"\"Quoted!\"\"\") f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)")   
    print(result)
  }  
  
  {
    val result: ParseResult[SequentialBranchPoint] = parseAll(Grammar.sequentialBranchPoint,"(a: 8..12)")   
    print(result)
  }  
    
  {
    // Complex example with line breaks
    val s:String =     
    """(taskName:
                 a=(1..20)
                 b=(
                   "The end of times"
                   "Hello, world"
                   "My goodness dear"
                   )
        )"""
    val result: ParseResult[BranchPointDef] = parseAll(Grammar.branchPoint,s)   
    print(result)
  }
  
  {
    val s:String =
          """moses tokenizerr giza
    < in=$out@tokenize[DataSet:test] > hyps"""
    val result: ParseResult[TaskHeader] = parseAll(Grammar.taskHeader,s)   
    print(result)   
  }
    
}