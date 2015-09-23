// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class UnquotedLiteralTest extends AbstractTest("unquoted literal",Grammar.unquotedLiteral) {
 
  def successCases = Set(
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "/path/to/something/cool"
  ) 
  
  def failureCases = Set(
      " ",      
      "Ends_with_paren)",
      ")",
      " starts_with_space",
      "contains spaces"
  ) 
  
  def errorCases = Set( 
      "(",
      "[",
      "]",
      "*",
      ":",
       "$",
      "@","@*(","foo*","bar@baz",
      "\"This is a quoted string\""
   
  )
  
}