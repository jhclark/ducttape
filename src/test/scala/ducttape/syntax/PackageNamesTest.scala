// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PackageNamesTest extends AbstractTest("package naames",Grammar.packageNameAssignment) {
 
  def successCases = Set(

    "moses",
    "tokenizer"
  ) 
  
  def failureCases = Set(
    "",
    " ",      
    """// Package comments
      moses tokenizerr giza""",
      "moses tokenizer giza"
  ) 
  
  def errorCases = Set(
      "moses=foo",
    "A-variable_Name__"  
  )
  
}