// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.util

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.GrammarParser._
import org.scalatest.Assertions
import scala.util.parsing.combinator.Parsers

object Tests {
  
  /** Verify that a test case succeeded. */
  def verify(testCase:Assertions, result:ParseResult[Any]) : Unit = { 
	  result match {
		  case Success(res, _) => ()
		  case Failure(msg, next) => testCase.fail("At position " + next.pos.toString + ": " + msg) //"At " + position+ ": "+ 
		  case Error(msg, next)   => testCase.fail("At position " + next.pos.toString + ": " + msg)//("At " + position+ ": "+ msg)
	  }
  }
  
  /** Verify that a test case failed in a way that the parser will not attempt to backtrack. */
  def verifyError(testCase:Assertions, result:ParseResult[Any]) : Unit = {
	  result match {
		  case Success(res, next) => testCase.fail("At position " + next.pos.toString + ": " + res.toString)
		  case Failure(msg, next) => testCase.fail("At position " + next.pos.toString + ": Encounted Failure instead of Error: " + msg)
		  case Error(msg, _)   => ()
	  }
  }
  
  /** Verify that a test case failed in a way that the parser will attempt to backtrack. */
  def verifyFailure(testCase:Assertions, result:ParseResult[Any]) : Unit = {
	  result match {
  		case Success(res, next) => testCase.fail("At position " + next.pos.toString + ": " + res.toString)
	  	case Failure(msg, _) => ()
		  case Error(msg, next)   => testCase.fail("At position " + next.pos.toString + ": Encounted Error instead of Failure: " + msg)
	  }
  }
  
}
