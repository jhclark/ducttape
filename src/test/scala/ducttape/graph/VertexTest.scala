// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.graph

import org.scalatest.FunSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
/*
@RunWith(classOf[JUnitRunner])
class VertexTest extends FunSpec {

  describe("A Vertex") {

    it("should store an integer identifier") {

      {
        val id = "42";
        val value = Some("Hello, World");
        val comment = Some("The meaning of life, the universe, and everything")
        val vertex = new Vertex(id,value,comment)

        assert(id.equals(vertex.id))
      }


      {
        val id = "42";
        val value = Some("Hello, World");
        val comment = None
        val vertex = new Vertex(id,value,comment)

        assert(id.equals(vertex.id))
      }

    }


    it("should store a value") {

      {
        val id = "42";
        val value = Some("Hello, World");
        val comment = Some("The meaning of life, the universe, and everything")
        val vertex = new Vertex(id,value,comment)

        vertex.contents match {
        case Some(id:String) => assert(true)
        case _ => fail("Vertex should store a String value, but does not")
        }

        assert(value.equals(vertex.contents))
      }


      {
        val id = "42";
        val value = Some("Hello, World");
        val comment = None
        val vertex = new Vertex(id,value,comment)

        vertex.contents match {
        case Some(value:String) => assert(true)
        case _ => fail("Vertex should store a String value, but does not")
        }

        assert(value.equals(vertex.contents))
      }

    }

    it("should optionally store a comment") {

      {
        val id = "42";
        val value = Some("Hello, World");
        val comment = Some("The meaning of life, the universe, and everything")
        val vertex = new Vertex(id,value,comment)

        vertex.comment match {
        case Some(id) => assert(true)
        case _ => fail("Vertex should store a String comment, but does not")
        }

        assert((comment.get).equals(vertex.comment.get))
      }


      {
        val id = "42";
        val value = Some("Hello, World");
        val comment = None
        val vertex = new Vertex(id,value,comment)

        vertex.comment match {
        case None => assert(true)
        case _ => fail("Vertex should store a String comment, but does not")
        }

        assert(comment.equals(vertex.comment))

        intercept[NoSuchElementException] {
          vertex.comment.get
        }
      }

    }

  }

}
*/
