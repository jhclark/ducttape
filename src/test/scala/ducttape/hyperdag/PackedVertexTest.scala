package ducttape.hyperdag

import org.scalatest.FunSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PackedVertexTest extends FunSpec {

  describe("A PackedVertex") {

    it("should store an integer identifier") {
      
      {
        val id = 42;
        val value = "Hello, World";
        val comment = Some("The meaning of life, the universe, and everything")
        val vertex = new PackedVertex(id,value,comment)

        assert(id.equals(vertex.id))
      }
      
      
      {
        val id = 42;
        val value = "Hello, World";
        val comment = None
        val vertex = new PackedVertex(id,value,comment)

        assert(id.equals(vertex.id))
      }
      
    }


    it("should store a value") {
      
      {
        val id = 42;
        val value = "Hello, World";
        val comment = Some("The meaning of life, the universe, and everything")
        val vertex = new PackedVertex(id,value,comment)

        vertex.value match {
        case id:String => assert(true)
        case _ => fail("PackedVertex should store a String value, but does not")
        }

        assert(value.equals(vertex.value))
      }
      
      
      {
        val id = 42;
        val value = "Hello, World";
        val comment = None
        val vertex = new PackedVertex(id,value,comment)

        vertex.value match {
        case value:String => assert(true)
        case _ => fail("PackedVertex should store a String value, but does not")
        }

        assert(value.equals(vertex.value))
      }
      
    }    
    
    it("should optionally store a comment") {
      
      {
        val id = 42;
        val value = "Hello, World";
        val comment = Some("The meaning of life, the universe, and everything")
        val vertex = new PackedVertex(id,value,comment)

        vertex.comment match {
        case Some(id) => assert(true)
        case _ => fail("PackedVertex should store a String comment, but does not")
        }

        assert((comment.get).equals(vertex.comment.get))
      }
      
      
      {
        val id = 42;
        val value = "Hello, World";
        val comment = None
        val vertex = new PackedVertex(id,value,comment)

        vertex.comment match {
        case None => assert(true)
        case _ => fail("PackedVertex should store a String comment, but does not")
        }

        assert(comment.equals(vertex.comment))
        
        intercept[NoSuchElementException] {
          vertex.comment.get
        }
      }
      
    }       
    
  }

}