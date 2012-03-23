package ducttape.hyperdag.walker

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import ducttape.hyperdag.HyperDagBuilder
import ducttape.hyperdag.HyperDag
import ducttape.hyperdag.UnpackedVertex
import org.junit.Test
import scala.collection.JavaConversions

@RunWith(classOf[JUnitRunner])
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
  
  val diamond: HyperDag[String,String,String] = {
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

    builder.build
  }

  it should "traverse a diamond with an iterator" in {
    val vertices = diamond.unpackedWalker.iterator.toList
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

  /**
	* This is a particularly important set of tests since the underlying implementation
	* of an unpacked DAG walker performs manual locking.
	*/
  it should "travse a diamond with an asynchronous walker" in {
    val timer = new Thread {
      override def run {
        import collection.JavaConversions._
        try {
          Thread.sleep(5000) // wait 5 seconds before detecting deadlock
        } catch {
          case e => ;
        }
        val m: Seq[(Thread,Array[StackTraceElement])] = Thread.getAllStackTraces.toSeq
        for ((thread: Thread, trace: Array[StackTraceElement]) <- m) {
          System.err.println(thread.getName)
          for(t <- trace) { System.err.println("   " + t.toString) } 
        }
        fail("Timed out")
      }
    }
    timer.setDaemon(false)
    timer.start
    
    for(i <- 0 until 1000) {
        //println(i)
	    val result = new collection.mutable.ListBuffer[String]
	    diamond.unpackedWalker.foreach(10, {v => result += v.packed.value})
	    //println(result)
    }
    timer.interrupt
  }
}