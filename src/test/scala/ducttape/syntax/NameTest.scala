// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NameTest extends AbstractTest("name",Grammar.name) {
 
  def successCases = Set(
    "A_task_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "abc",
    "z123_45charlie"
  ) 
  
  def failureCases = Set(
    ""
  ) 
  
  def errorCases = Set(
    " ",
    "[]",
    "A-variable_Name__",
    "[A_task_Name__    ]",
    "[  A_task_Name__]",
    "[ A_task_Name__ ]",
    "[A_task_Name__ ",
    "[A_task_Name__]",
    "[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_]",
    "[abc]",
    "[z123_45charlie]"    
  )
  
}