package ducttape.graph

import scala.collection.mutable.HashMap
import ducttape.syntax.BashCode
import ducttape.syntax.AbstractSyntaxTree.RValue

/**
 *
 *
 * @author Lane Schwartz
 * @author Jonathan Clark
 */
class Vertex(val id:String, val vertexType:VertexType, val contents:Any, val comment: Option[String] = None) {

  if (id==null) throw new NullPointerException("Value id was initialized as null. This is forbidden.")

  private[graph] val incomingEdges = new HashMap[String, Edge]
  private[graph] val outgoingEdges = new HashMap[String, Edge]


  override def hashCode() = id.hashCode()

  override def equals(that: Any) = that match { case other: Vertex => (other.id.equals(this.id)) }

  override def toString() = comment match {
    case Some(str) => "%s\t%s:%d".format(vertexType, str, id)
    case None => "%s:%d".format(vertexType, id)
  }

}



sealed trait VertexType

object VertexType {
  case object Task extends VertexType
  case object TaskInput extends VertexType
  case object TaskParam extends VertexType
  case object TaskOutput extends VertexType
}


class TaskVertex
