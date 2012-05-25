package ducttape.hyperdag.walker

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import ducttape.hyperdag.HyperDagBuilder
import ducttape.hyperdag.HyperDag
import ducttape.hyperdag.UnpackedVertex
import org.junit.Test
import scala.collection.JavaConversions
import grizzled.slf4j.Logging

@RunWith(classOf[JUnitRunner])
class UnpackedDagWalkerTest extends FlatSpec with Logging{

  "An Unpacked DAG Walker" should "traverse a linear chain graph with an iterator" in {

    val builder = new HyperDagBuilder[String,String,String]
    val a = builder.addVertex("Vertex A")
    val b = builder.addVertex("Vertex B")
    val c = builder.addVertex("Vertex C")

    val e1 = builder.addHyperEdge("HyperEdge 1", List((a,"")), b)
    val e2 = builder.addHyperEdge("HyperEdge 2", List((b,"")), c)

    val dag: HyperDag[String,String,String] = builder.build
    val vertices = dag.unpackedWalker().iterator.toList
    for (v: UnpackedVertex[String,String,String,String] <- vertices) {
      debug(v)
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
    val vertices = diamond.unpackedWalker().iterator.toList
    for (v: UnpackedVertex[String,String,String,String] <- vertices) {
      debug(v)
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
    val DEADLOCK_TIMEOUT = 10000 // 10 sec
    
    val timer = new Thread {
      override def run {
        import collection.JavaConversions._
        try {
          Thread.sleep(DEADLOCK_TIMEOUT)
          val m: Seq[(Thread,Array[StackTraceElement])] = Thread.getAllStackTraces.toSeq
          for ( (thread: Thread, trace: Array[StackTraceElement]) <- m) {
            System.err.println(thread.getName)
            for(t <- trace) { System.err.println("   " + t.toString) } 
          }
          fail("Timed out: Potential deadlock")
        } catch {
          case e: InterruptedException => ;
        }
      }
    }
    timer.setDaemon(false)
    timer.start()
    
    val order = new Ordering[Seq[String]] {
      override def compare(a: Seq[String], b: Seq[String]): Int = {
        if (a.size == b.size) {
          a.zip(b).find{case (aa,bb) => aa != bb} match {
            case Some((aa, bb)) => aa.compareTo(bb)
            case None => 0
          }
        } else {
          a.size.compareTo(b.size)
        }
      }
    }
    
    val expectedReals = List(
    	Seq(),
    	Seq("HyperEdge 1"),
    	Seq("HyperEdge 2"),
    	Seq("HyperEdge 3", "HyperEdge 1"),
    	Seq("HyperEdge 2", "HyperEdge 4"),
    	Seq("HyperEdge 2", "HyperEdge 5", "HyperEdge 1")
    ).sorted(order)
    
    val expectedVerts = List("Vertex A", "Vertex B", "Vertex C", "Vertex D").sorted
    
    def listsEqual(a: Seq[Seq[String]], b: Seq[Seq[String]]): Boolean = {
      if (a.size != b.size) {
        debug("Outer length %d != %d".format(a.size, b.size))
        false
      } else {
        a.zip(b).forall { case (seqA, seqB) =>
          if (seqA.size != seqB.size) {
            debug("Inner length %d != %d".format(seqA.size, seqB.size))
            false
          } else {
            seqA.zip(seqB).forall { case (strA, strB) =>
              if (strA != strB) {
                debug("Not equal: %s and %s".format(strA, strB))
              }
              strA == strB
            }
          }
        }
      }
    }
    
    for (i <- 0 until 1000) {
	    val verts = new collection.mutable.HashSet[String]
	    val reals = new collection.mutable.HashSet[Seq[String]]
	    diamond.unpackedWalker().foreach(10, {v =>
	      verts += v.packed.value
	      reals += v.realization
	    })
	    val vertList = verts.toList.sorted
	    assert(vertList == expectedVerts, "Wrong vertices produced: " + verts)
	    val realList = reals.toList.sorted(order)
	    //debug("Expected: " + expectedReals)
	    //debug("Actual  : " + realList)
	    // == was not calling deep equals on strings! can we override with an implicit?
	    assert(listsEqual(realList, expectedReals), "Wrong realizations produced: " + realList)
    }
    timer.interrupt()
  }
}
