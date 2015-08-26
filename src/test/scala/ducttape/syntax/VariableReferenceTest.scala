// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class VariableReferenceTest extends AbstractTest("variable name",Grammar.variableReference) {
 
  def successCases = Set(
    "$A_variable_Name__",
    "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "$abc"
  ) 
  
  def failureCases = Set(
    "",
    " ",
    "A-variable_Name__",
    "abc"
  ) 
  
  def errorCases = Set(
    "$A-variable_Name__",      
    "$ "
  )
  
}