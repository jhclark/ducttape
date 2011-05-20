import org.scalatest.FlatSpec
import scala.collection.mutable.Stack

import ducttape.hyperdag._

class ExampleSpec extends FlatSpec {

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

class RDagTest extends FlatSpec {

  "A Realization DAG" should "pop values in last-in-first-out order" is (pending)

  it should "throw NoSuchElementException if an empty stack is popped" is (pending)
}

class PackedDagWalkerTest extends FlatSpec {

  "A Packed DAG Walker" should "traverse a linear chain" is (pending)

  it should "travse a diamond" is (pending)
}
