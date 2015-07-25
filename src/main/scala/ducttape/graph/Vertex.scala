package ducttape.graph

import scala.collection.mutable.HashMap

import ducttape.syntax.AbstractSyntaxTree.RValue
import ducttape.syntax.AbstractSyntaxTree.TaskDef

/**
 *
 *
 * @author Lane Schwartz
 * @author Jonathan Clark
 */
abstract sealed class Vertex(val id:String, val comment: Option[String] = None) {

  val contents:Any

  if (id==null) throw new NullPointerException("Value id was initialized as null. This is forbidden.")

  private[graph] val incomingEdges = new HashMap[String, Edge]
  private[graph] val outgoingEdges = new HashMap[String, Edge]


  override def hashCode() = id.hashCode()

  override def equals(that: Any) = that match { case other: Vertex => (other.id.equals(this.id)) }

  override def toString() = comment match {
    case Some(str) => "%s:%d".format(str, id)
    case None => "ID=%d".format(id)
  }

}



class TaskVertex(id:String, val contents:TaskDef, comment: Option[String] = None) extends Vertex(id, comment)

abstract class TaskSpecVertex(id:String, comment:Option[String]=None) extends Vertex(id,comment)
class TaskInputVertex(id:String, val contents:RValue, comment: Option[String] = None) extends TaskSpecVertex(id, comment)
class TaskOutputVertex(id:String, val contents:RValue, comment: Option[String] = None) extends TaskSpecVertex(id, comment)
class TaskParamVertex(id:String, val contents:RValue, comment: Option[String] = None) extends TaskSpecVertex(id, comment)
