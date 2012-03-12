package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NumberTest extends AbstractTest("number",Grammar.number) {
 
  def successCases = Set(
      "1",
      "-1",
      "+1",
      "+1.0",
      "-1.0",
      "+1.0E10",
      "-1.0E10",
      "+1.0E-10",
      "-1.0E-10",      
      "3.141526535897923384626433832795028841971693993751058209",
      "123456789012345678901234567890123456789012345678901234567890",
      "123456789012345678901234567890123456789012345678901234567890e2147483647",
      "123456789012345678901234567890123456789012345678901234567890e-2147483647",
      "0",
      "0.00",
      "123",
      "-123",
      "1.23E3",
      "1.23E+3",
      "12.3E+7",
      "12.0",
      "12.3",
      "0.00123",
      "-1.23E-12",
      "1234.5E-4",
      "0E+7",
      "-0" 
  ) 
  
  def failureCases = Set(
    "",
    " ",
    "NaN",
    "Infinity",
    "-Infinity",
    "+Infinity",
    "3.14e3.14",
    "A-variable_Name__",
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "!@#$%^&*()"
  ) 
  
  def errorCases = Set(
    ".05",
    "-.9",
    "+.8",      
    "10e-2147483648",
    "10e2147483648",
    "123456789012345678901234567890123456789012345678901234567890e123456789012345678901234567890123456789012345678901234567890"
  )
  
}