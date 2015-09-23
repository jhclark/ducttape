// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import scala.collection.mutable.Stack
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SampleTest extends FlatSpec {

	"A Stack" should "pop values in last-in-first-out order PEND" is (pending)

	it should "throw NoSuchElementException if an empty stack is popped PEND" is (pending)

	it should "pop values in last-in-first-out order" in {
		val stack = new Stack[Int]
		                      stack.push(1)
		                      stack.push(2)
		                      assert(stack.pop() === 2)
		                      assert(stack.pop() === 1)
	}

	it should "throw NoSuchElementException if an empty stack is popped" in {
		val emptyStack = new Stack[String]
		                           intercept[NoSuchElementException] {
			emptyStack.pop()
		}
	}

}