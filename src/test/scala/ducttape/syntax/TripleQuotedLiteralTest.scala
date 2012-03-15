package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TripleQuotedLiteralTest extends AbstractTest("triple quoted literal",Grammar.tripleQuotedLiteral) {

  def successCases = Set(
    "\"\"\"" + "This is a quoted string" + "\"\"\"",
    "\"\"\"" + """This is a quoted string with \r lots of \\u garbage! \b in it!""" + "\"\"\"",    
    "\"\"\"This has line breaks in\nit!\"\"\"",
    "\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\"",
    "\"\"\"This string ends with two closing quotes\"\"\"\"\""
  ) 
  
  def failureCases = Set(
    """"This is a quoted string"""",
    """'This one uses single quotes '""",
    """' Escape\tsequences\nare\rallowed! '""",
    """"She said, \"Welcome to Sleepy Hollow!\""""",
    """'I say that she\'s the best.'""",
    "\"Unicode sequences should be fine \u21AF too\"",
    "\'Unicode sequences should be fine \u2231 too\'",
    """'It\'s a bad idea to use it, but a backspace literal \b is allowed'"""      
  ) 
  
  def errorCases = Set(
    "\"\"\"This is a badly quoted string",
    "\"\"\"This is a badly quoted string\"",
    "\"\"\"This is a badly quoted string\"\"",
    "\"\"\"This is a badly quoted string'",
    "\"\"\"This is a badly quoted string''",
    "\"\"\"This is a badly quoted string'''",
    "\"\"\"This has too many closing quotes\"\"\"\"\"\""
  )
  
}