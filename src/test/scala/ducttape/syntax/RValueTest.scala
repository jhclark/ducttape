package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RValueTest extends AbstractTest("rvalue",Grammar.rvalue) {
 
  def successCases = Set(
      
    // Sequential branch point
    """(branchPointName: 1..5)""",   
    """(branchPointName: 1..5.0)""",
    """(branchPointName: 1.0..5.0)""",
    """(branchPointName: 10e1..10e999)""",
    """(branchPointName: 9.9e256..7.7e1024)""",
    """(branchPointName: 1..6..2)""",   
    """(branchPointName: 1..5.0..0.5)""",
    """(branchPointName: 1.0..5.0..2)""",
    """(branchPointName: 10e1..10e999..1)""",
    """(branchPointName: 9.9e256..7.7e1024..5.4e3)""",      
      
    // Branch graft
    "$variableName@taskName[branchPointName:branchName]",
    "$variableName@taskName[branchPointName:*]",
    "$variableName@taskName[a:b,c:d]",
    "$variableName@taskName[a1:b2,c3:*,d4:e5,f6:g7]",
      
    // Task variable reference
    "$A_variable_Name__@foo",
    "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_@barBar",
    "$abc@def",
      
    // Variable reference
    "$A_variable_Name__",
    "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "$abc",
    
    // Unquoted literal
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "/path/to/something/cool",
    "abc",
    "A_variable_Name__:",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_:",
    "A-variable_Name__",
    "A_variable_Name__",
    
    // Quoted literal
    """"This is a quoted string"""",
    """'This one uses single quotes '""",
    """' Escape\tsequences\nare\rallowed! '""",
    "\"Unicode sequences should be fine \u21AF too\"",
    "\'Unicode sequences should be fine \u2231 too\'"    
  ) 
  
  def failureCases = Set(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_ :",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_     :" 
  ) 
  
  def errorCases = Set(
    "",      
    " ",      
    """"This is a badly quoted string\"""",
    """"This one is, too"it seems"""",
    """'Starting with a single and ending with a double"""",
    """"Starting with a double and ending with a single'""",    
    "$variableName@taskName[",
    "$A-variable_Name__",
    "$"
  )
  
}