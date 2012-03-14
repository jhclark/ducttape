package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BranchPointTest extends AbstractTest("branch point",Grammar.branchPoint) {
 
  def successCases = Set(
      """(branchPointName: a=1)""",
      """(branchPointName: a=1 b=5)""",   
      """(greeting: y="welcome home" z="bugger off")""",
      """(sauce: a1="A1 Sauce" ketchup="Tomato Ketchup" wasabi="wasabi")""",
      "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=\"kumbaya\" )",
      "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=\"kumbaya\")",
      "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=kumbaya)"      
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
    "!@#$%^&*()",
      "1",
      "-1",
      "+1",
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
      "-0"     ,
    "10e-2147483648",
    "10e2147483648",
    "123456789012345678901234567890123456789012345678901234567890e123456789012345678901234567890123456789012345678901234567890"
      
  ) 
  
  def errorCases = Set(
    """(branchPointName: .1..5)""",   
    """(branchPointName: 1..-.05)""",
    """(branchPointName: 1.0...5)""",
    """(branchPointName: -.10e1..10e999)""",
    """(branchPointName: .9e256..7.7e1024)"""      
  )
  
}