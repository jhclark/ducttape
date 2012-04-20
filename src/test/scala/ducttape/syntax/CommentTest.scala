package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CommentTest extends AbstractTest("comment",Grammar.comment) {
 
  def successCases = Set(
    "// Hello, world",
    "#  Hello, world",
    "// This is OK /* Unclosed ",
    "#  This is OK /* Unclosed ",
    "/**/",
    "/*a*/",
    "/* A_variable_Name__ */",
    "/* ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_ */",
    "/* Here we go /* another problem */ */",
    """/*
    
    space!
    
    */""",
  """/*
    
    /*
    space!
    */
    
    */""",
  """/*
    
    /*
    space!
    *//*and more*/
    
    */"""    
  ) 
  
  def failureCases = Set(
    "",
    " ",
    "A-variable_Name__"    
  ) 
  
  def errorCases = Set(
    "/* Forgot to close ",
    "/* Here we go /* another problem",
    "/* Here we go /* another problem */"
  )
  
}