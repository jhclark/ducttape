package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BareNameTest extends AbstractTest("variable name",Grammar.name("""\z""".r)) {
 
  def successCases = Set(
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_"
  ) 
  
  def failureCases = Set(
    ""
  ) 
  
  def errorCases = Set(
    " ",
    "A-variable_Name__"
  )
  
}