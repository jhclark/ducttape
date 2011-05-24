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

  "A Packed DAG Walker" should "traverse a linear chain graph with an iterator" in {

    val builder = new PackedDagBuilder[String,String,String]
    val a = builder.add("Vertex A")
    val b = builder.add("Vertex B")
    val c = builder.add("Vertex C")

    val e1 = builder.add("HyperEdge 1", List(), List(a), b)
    val e2 = builder.add("HyperEdge 2", List(), List(b), c)

    val dag: PackedDag[String,String,String] = builder.build
    println(dag.toGraphViz)
    val vertices = for(v <- dag.walker.iterator.toList) yield v.value
    assert(vertices(0) == "Vertex A")
    assert(vertices(1) == "Vertex B")
    assert(vertices(2) == "Vertex C")
  }

  it should "traverse a diamond with an iterator" in {

    val builder = new PackedDagBuilder[String,String,String]
    val a = builder.add("Vertex A")
    val b = builder.add("Vertex B")
    val c = builder.add("Vertex C")
    val d = builder.add("Vertex D")

    builder.add("HyperEdge 1", List(), List(a), b)
    builder.add("HyperEdge 2", List(), List(a), c)
    builder.add("HyperEdge 3", List(), List(b), d)
    builder.add("HyperEdge 4", List(), List(c), d)

    val dag: PackedDag[String,String,String] = builder.build
    GraphViz.compile(dag.toGraphViz, "ex.pdf")
    val vertices = for(v <- dag.walker.iterator.toList) yield v.value
    assert(vertices(0) == "Vertex A")
    assert(vertices(1) == "Vertex B")
    assert(vertices(2) == "Vertex C")
    assert(vertices(3) == "Vertex D")
  }

  it should "travse a linear chain graph with an asynchronous walker" is (pending)

  it should "travse a diamond with an asynchronous walker" is (pending)
}
