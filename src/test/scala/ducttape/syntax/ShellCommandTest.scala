package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ShellCommandTest extends AbstractTest("shell command",Grammar.shellCommand) {
 
  def successCases = Set(
    "",
    " ",
    "A-variable_Name__",  
    "moses tokenizer giza",
    "moses",
    " }",
    """ruby -e 'puts "#{month_string.upcase}(#{today.year}-#{month_num})#{month_string.downcase}"'"""
  ) 
  
  def failureCases = Set(
    "}",
    """# Package comments
      moses tokenizerr giza"""    
  ) 
  
  def errorCases = Set(
    
  )
  
}