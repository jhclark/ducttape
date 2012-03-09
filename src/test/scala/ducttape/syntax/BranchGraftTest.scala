package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BranchGraftTest extends AbstractTest("branch graft",Grammar.branchGraft) {
 
  def successCases = Set(
      "$variableName@taskName[branchPointName:branchName]",
      "$variableName@taskName[branchPointName:*]",
      "$variableName@taskName[a:b,c:d]",
      "$variableName@taskName[a1:b2,c3:*,d4:e5,f6:g7]"
  ) 
  
  def failureCases = Set(
    "",
    " ",
    "$variableName@taskName[",
    "$A_variable_Name__",
    "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "$abc",
    "abc",
    "A_variable_Name__:",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_:",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_ :",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_     :",
    "A-variable_Name__",
    "A_variable_Name__",
    """"This is a quoted string"""",
    """'This one uses single quotes '""",
    """' Escape\tsequences\nare\rallowed! '""",
    "\"Unicode sequences should be fine \u21AF too\"",
    "\'Unicode sequences should be fine \u2231 too\'",
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "/path/to/something/cool",
    """"This is a badly quoted string\"""",
    """"This one is, too"it seems"""",
    """'Starting with a single and ending with a double"""",
    """"Starting with a double and ending with a single'"""    
  ) 
  
  def errorCases = Set(
    "$A-variable_Name__",
    "$ "    
  )
  
}