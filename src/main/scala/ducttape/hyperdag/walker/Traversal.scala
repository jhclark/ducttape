package ducttape.hyperdag.walker

import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.UnpackedVertex

import java.util.Comparator
import collection._

trait Traversal

case object Arbitrary extends Traversal {
	val comparator = new Comparator[UnpackedVertex[_,_,_,_]]() {
		// Always return zero
		def compare(o1:UnpackedVertex[_,_,_,_], o2:UnpackedVertex[_,_,_,_]) = 0
	}
	override def toString = "arbitrary"
}

case object BreadthFirst extends Traversal {
	def comparator(vertexIDs: Map[(PackedVertex[_],Seq[_]),Int]) = new Comparator[UnpackedVertex[_,_,_,_]]() {
		def compare(o1:UnpackedVertex[_,_,_,_], o2:UnpackedVertex[_,_,_,_]) : Int = {
			val id1 = vertexIDs( (o1.packed,o1.realization) )
			val id2 = vertexIDs( (o2.packed,o2.realization) )
			return id1.compareTo(id2)
		}
	}
	override def toString = "breadth-first"
}


case object DepthFirst extends Traversal {
	def comparator(vertexIDs: Map[(PackedVertex[_],Seq[_]),Int]) = new Comparator[UnpackedVertex[_,_,_,_]]() {
		def compare(o1:UnpackedVertex[_,_,_,_], o2:UnpackedVertex[_,_,_,_]) : Int = {
			val id1 = vertexIDs( (o1.packed,o1.realization) )
			val id2 = vertexIDs( (o2.packed,o2.realization) )
			return id2.compareTo(id1)
		}
	}
	override def toString = "depth-first"
}

