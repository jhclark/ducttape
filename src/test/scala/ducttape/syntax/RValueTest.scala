package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RValueTest extends AbstractTest("rvalue",Grammar.rvalue) {
 
  def successCases = Set(

    // Branch point
    """(branchPointName: a=1)""",
    """(branchPointName: a=1 b=5)""",   
    """(greeting: y="welcome home" z="bugger off")""",
    """(sauce: a1="A1 Sauce" ketchup="Tomato Ketchup" wasabi="wasabi")""",
    "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=\"kumbaya\" )",
    "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=\"kumbaya\")",
    "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=kumbaya)",      
      
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
    "A-variable_Name__",
    "A_variable_Name__",
    
    // Quoted literal
    """"This is a quoted string"""",
    """'This one uses single quotes '""",
    """' Escape\tsequences\nare\rallowed! '""",
    "\"Unicode sequences should be fine \u21AF too\"",
    "\'Unicode sequences should be fine \u2231 too\'",   
    
    // Triple quoted literal
    "\"\"\"" + "This is a quoted string" + "\"\"\"",
    "\"\"\"" + """This is a quoted string with \r lots of \\u garbage! \b in it!""" + "\"\"\"",    
    
    // Complex nesting
    "(a: a1=(k: 8..12) a4=7)",
    "(a: a1=$g@taskH[i:j])",
    "(a: a1=(b: f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",      
    "(a: a1=(b: c=$d@taskE) a2=5 a3=(k: 8..12) a4=7)",      
    "(a: a1=(b: c=$d@taskE f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",
    "(a: a1=(b: c=(x: x1=$d@taskE x2=farOut x3=\"\"\"Quoted!\"\"\") f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)"    
    
  ) 
  
  def failureCases = Set(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_ :",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_     :"
  ) 
  
  def errorCases = Set(
    "$variableName@taskName[",
    "$A-variable_Name__",
    "$",
    "A_variable_Name__:",      
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_:",
    """"This is a badly quoted string\"""",
    """"This one is, too"it seems"""",
    """'Starting with a single and ending with a double"""",
    """"Starting with a double and ending with a single'""",    
    " ",
    "",
    "\"\"\"This is a badly quoted string",
    "\"\"\"This is a badly quoted string\"",
    "\"\"\"This is a badly quoted string\"\"",
    "\"\"\"This is a badly quoted string'",
    "\"\"\"This is a badly quoted string''",
    "\"\"\"This is a badly quoted string'''"    
  )
  
}