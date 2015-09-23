// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.hyperdag.walker

import org.scalatest.FlatSpec
import ducttape.hyperdag.HyperDagBuilder
import ducttape.hyperdag.HyperDag
import ducttape.viz.GraphViz
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import grizzled.slf4j.Logging

@RunWith(classOf[JUnitRunner])
class PackedDagWalkerTest extends FlatSpec with Logging {
  
  "A Packed DAG Walker" should "traverse a linear chain graph with an iterator" in {

    val builder = new HyperDagBuilder[String,String,String]
    val a = builder.addVertex("Vertex A")
    val b = builder.addVertex("Vertex B")
    val c = builder.addVertex("Vertex C")

    val e1 = builder.addHyperEdge("HyperEdge 1", List((a,"")), b)
    val e2 = builder.addHyperEdge("HyperEdge 2", List((b,"")), c)

    val dag: HyperDag[String,String,String] = builder.build
    info(dag.toGraphViz)
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
    //GraphViz.compile(dag.toGraphViz, "ex.pdf")
    val vertices = for(v <- dag.packedWalker.iterator.toList) yield v.value
    assert(vertices(0) == "Vertex A")
    assert(vertices(1) == "Vertex B")
    assert(vertices(2) == "Vertex C")
    assert(vertices(3) == "Vertex D")
  }

  it should "travse a linear chain graph with an asynchronous walker" is (pending)

  it should "travse a diamond with an asynchronous walker" is (pending)
}