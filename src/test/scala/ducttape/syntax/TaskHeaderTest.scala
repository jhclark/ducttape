package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TaskHeaderTest extends AbstractTest("task header",Grammar.taskHeader) {
 
  def successCases = Set(
//    "[hello_world]",
//    "[hello_world_2] > x y_txt",
//    "[hello_world_3] < a=/etc/passwd b=/etc/hosts",
//    "[first] > x",
//    "[and_then] < a=$x@first > x",
//    "[tokenize] tokenizer < in=(DataSet: train=a.txt tune=b.txt test=c.txt) > out",
//    "[build_model] < in=$out@tokenize[DataSet:train] > model",
//    "[optimize] < in=$out@tokenize[DataSet:tune] > weights",
//    "[test] < in=$out@tokenize[DataSet:test] > hyps"
    "",
    " ",      
    "> x y_txt",
    "< a=/etc/passwd b=/etc/hosts",
    "> x",
    "< a=$x@first > x",
    "tokenizer < in=(DataSet: train=a.txt tune=b.txt test=c.txt) > out",
    "< in=$out@tokenize[DataSet:train] > model",
    "< in=$out@tokenize[DataSet:tune] > weights",
    "moses tokenizerr giza < in=$out@tokenize[DataSet:test] > hyps",
    """moses tokenizerr giza
    < in=$out@tokenize[DataSet:test] > hyps""",
    """moses tokenizerr giza
    // Do some inputs
    < in=$out@tokenize[DataSet:test] 
    // Here's the result
    > hyps""",
    """// Package comments
      moses tokenizerr giza
    // Do some inputs
    < in=$out@tokenize[DataSet:test] 
    // Here's the result
    > hyps"""  
  ) 
  
  def failureCases = Set(
 
  ) 
  
  def errorCases = Set(
    "A-variable_Name__"
  )
  
}