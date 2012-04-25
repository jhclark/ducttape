package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class VariableReferenceTest extends AbstractTest("variable name",Grammar.variableReference) {
 
  def successCases = Set(
    "$A_variable_Name__",
    "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "$abc"
  ) 
  
  def failureCases = Set(
    "",
    " ",
    "A-variable_Name__",
    "abc"
  ) 
  
  def errorCases = Set(
    "$A-variable_Name__",      
    "$ "
  )
  
}