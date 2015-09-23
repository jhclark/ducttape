// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
    """"She said, \"Welcome to Sleepy Hollow!\""""",
    """'I say that she\'s the best.'""",
    "\"Unicode sequences should be fine \u21AF too\"",
    "\'Unicode sequences should be fine \u2231 too\'",
    """"Unicode sequences should be fine \"""+"""u21AF too"""",
    """'Unicode sequences should be fine \"""+"""u2231 too'""",
    """'It\'s a bad idea to use it, but a backspace literal \b is allowed'"""
  ) 
  
  def failureCases = Set(
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "/path/to/something/cool",
    " "
  ) 
  
  def errorCases = Set(
    """"This is a badly quoted string\"""",
    """'Starting with a single and ending with a double"""",
    """"Starting with a double and ending with a single'""",      
    """"This one is, too"it seems""""   
  )
  
}