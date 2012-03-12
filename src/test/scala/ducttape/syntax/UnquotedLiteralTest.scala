package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class UnquotedLiteralTest extends AbstractTest("unquoted literal",Grammar.unquotedLiteral) {
 
  def successCases = Set(
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "/path/to/something/cool"
  ) 
  
  def failureCases = Set(
  ) 
  
  def errorCases = Set(
    "\"This is a quoted string\"",
    " "      
  )
  
}