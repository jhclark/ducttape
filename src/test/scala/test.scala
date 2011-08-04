import org.scalatest.FlatSpec
import scala.collection.mutable.Stack

import ducttape.hyperdag._
import ducttape.viz._

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

    val builder = new HyperDagBuilder[String,String,String]
    val a = builder.addVertex("Vertex A")
    val b = builder.addVertex("Vertex B")
    val c = builder.addVertex("Vertex C")

    val e1 = builder.addHyperEdge("HyperEdge 1", List((a,"")), b)
    val e2 = builder.addHyperEdge("HyperEdge 2", List((b,"")), c)

    val dag: HyperDag[String,String,String] = builder.build
    println(dag.toGraphViz)
    val vertices = for(v <- dag.packedWalker.iterator.toList) yield v.value
    assert(vertices(0) == "Vertex A")
    assert(vertices(1) == "Vertex B")
    assert(vertices(2) == "Vertex C")
  }

  it should "traverse a diamond with an iterator" in {

    val builder = new HyperDagBuilder[String,String,String]
    val a = builder.addVertex("Vertex A")
    val b = builder.addVertex("Vertex B")
    val c = builder.addVertex("Vertex C")
    val d = builder.addVertex("Vertex D")

    builder.addHyperEdge("HyperEdge 1", List((a,"")), b)
    builder.addHyperEdge("HyperEdge 2", List((a,"")), c)
    builder.addHyperEdge("HyperEdge 3", List((b,"")), d)
    builder.addHyperEdge("HyperEdge 4", List((c,"")), d)

    val dag: HyperDag[String,String,String] = builder.build
    GraphViz.compile(dag.toGraphViz, "ex.pdf")
    val vertices = for(v <- dag.packedWalker.iterator.toList) yield v.value
    assert(vertices(0) == "Vertex A")
    assert(vertices(1) == "Vertex B")
    assert(vertices(2) == "Vertex C")
    assert(vertices(3) == "Vertex D")
  }

  it should "travse a linear chain graph with an asynchronous walker" is (pending)

  it should "travse a diamond with an asynchronous walker" is (pending)
}

class UnpackedDagWalkerTest extends FlatSpec {

  "An Unpacked DAG Walker" should "traverse a linear chain graph with an iterator" in {

    val builder = new HyperDagBuilder[String,String,String]
    val a = builder.addVertex("Vertex A")
    val b = builder.addVertex("Vertex B")
    val c = builder.addVertex("Vertex C")

    val e1 = builder.addHyperEdge("HyperEdge 1", List((a,"")), b)
    val e2 = builder.addHyperEdge("HyperEdge 2", List((b,"")), c)

    val dag: HyperDag[String,String,String] = builder.build
    val vertices = dag.unpackedWalker.iterator.toList
    for(v: UnpackedVertex[String,String,String] <- vertices) {
      println(v)
    }
    assert(vertices(0).packed.value == "Vertex A")
    assert(vertices(1).packed.value == "Vertex B")
    assert(vertices(2).packed.value == "Vertex C")
  }

  it should "traverse a diamond with an iterator" in {
    val builder = new HyperDagBuilder[String,String,String]
    val a = builder.addVertex("Vertex A")
    val b = builder.addVertex("Vertex B")
    val c = builder.addVertex("Vertex C")
    val d = builder.addVertex("Vertex D")

    builder.addHyperEdge("HyperEdge 1", List((a,"")), b)
    builder.addHyperEdge("HyperEdge 2", List((a,"")), c)
    builder.addHyperEdge("HyperEdge 3", List((b,"")), d)
    builder.addHyperEdge("HyperEdge 4", List((c,"")), d)
    builder.addHyperEdge("HyperEdge 5", List((b,""), (c,"")), d)

    val dag: HyperDag[String,String,String] = builder.build
    val vertices = dag.unpackedWalker.iterator.toList
    for(v: UnpackedVertex[String,String,String] <- vertices) {
      println(v)
    }
    
    assert(vertices(0).packed.value == "Vertex A")
    assert(vertices(1).packed.value == "Vertex B")
    assert(vertices(2).packed.value == "Vertex C")
    assert(vertices(3).packed.value == "Vertex D")
    // TODO: Make sure each vertex is only traversed once
  }

  it should "travse a linear chain graph with an asynchronous walker" is (pending)

  it should "travse a diamond with an asynchronous walker" is (pending)
}

class PackedMetaDagWalkerTest extends FlatSpec {

  "A Packed MetaDAG Walker" should "traverse a linear chain graph with an iterator" in {

    val builder = new HyperDagBuilder[String,String,String]
    val a = builder.addVertex("Vertex A")
    val b = builder.addVertex("Vertex B")
    val c = builder.addVertex("Vertex C")

    val e1 = builder.addHyperEdge("HyperEdge 1", List((a,"")), b)
    val e2 = builder.addHyperEdge("HyperEdge 2", List((b,"")), c)

    val dag: HyperDag[String,String,String] = builder.build
    println(dag.toGraphViz)
    val vertices = for(v <- dag.packedWalker.iterator.toList) yield v.value
    assert(vertices(0) == "Vertex A")
    assert(vertices(1) == "Vertex B")
    assert(vertices(2) == "Vertex C")
  }

  it should "traverse a diamond with an iterator" in {

    val builder = new HyperDagBuilder[String,String,String]
    val a = builder.addVertex("Vertex A")
    val b = builder.addVertex("Vertex B")
    val c = builder.addVertex("Vertex C")
    val d = builder.addVertex("Vertex D")

    builder.addHyperEdge("HyperEdge 1", List((a,"")), b)
    builder.addHyperEdge("HyperEdge 2", List((a,"")), c)
    builder.addHyperEdge("HyperEdge 3", List((b,"")), d)
    builder.addHyperEdge("HyperEdge 4", List((c,"")), d)

    val dag: HyperDag[String,String,String] = builder.build
    GraphViz.compile(dag.toGraphViz, "ex.pdf")
    val vertices = for(v <- dag.packedWalker.iterator.toList) yield v.value
    assert(vertices(0) == "Vertex A")
    assert(vertices(1) == "Vertex B")
    assert(vertices(2) == "Vertex C")
    assert(vertices(3) == "Vertex D")
  }

  it should "travse a linear chain graph with an asynchronous walker" is (pending)

  it should "travse a diamond with an asynchronous walker" is (pending)
}
