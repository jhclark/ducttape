package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NameTest extends AbstractTest("name",Grammar.name) {
 
  def successCases = Set(
    "A_task_Name__:",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_:",
    "abc:",
    "z123_45charlie:"
  ) 
  
  def failureCases = Set(
    ""
  ) 
  
  def errorCases = Set(
    " ",
    "[]",
    "A-variable_Name__",
    "[A_task_Name__    ]",
    "[  A_task_Name__]",
    "[ A_task_Name__ ]",
    "[A_task_Name__ ",
    "[A_task_Name__]",
    "[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_]",
    "[abc]",
    "[z123_45charlie]"    
  )
  
}