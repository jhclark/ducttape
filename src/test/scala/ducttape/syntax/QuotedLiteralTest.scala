package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class QuotedLiteralTest extends AbstractTest("quoted literal",Grammar.quotedLiteral) {
 
  def successCases = Set(
    """"This is a quoted string"""",
    """'This one uses single quotes '""",
    """' Escape\tsequences\nare\rallowed! '""",
    "\"Unicode sequences should be fine \u21AF too\"",
    "\'Unicode sequences should be fine \u2231 too\'"
  ) 
  
  def failureCases = Set(
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "/path/to/something/cool",
    " ",
    """"This is a badly quoted string\"""",
    """"This one is, too"it seems"""",
    """'Starting with a single and ending with a double"""",
    """"Starting with a double and ending with a single'"""
  ) 
  
  def errorCases = Set(
  )
  
}