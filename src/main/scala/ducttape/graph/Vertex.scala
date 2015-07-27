package ducttape.graph

import scala.collection.mutable.HashMap

import ducttape.syntax.AbstractSyntaxTree._

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



case class RootVertex() extends Vertex(id="", comment=None) { val contents=None }


//class ConfigVertex(val contents:ConfigDefinition)
//  extends Vertex(
//    id=contents.name match {
//      case Some(s) => contents.keyword + " " + s
//      case None    => contents.keyword
//    },
//    contents.comments.value
//  )

class LiteralVertex(val contents:Literal) extends Vertex(id=contents.value, comment=None)

sealed trait ParamVertex
class ConfigParamVertex(val contents : ConfigAssignment) extends Vertex(id=contents.spec.name, comment=contents.comments.value) with ParamVertex

class TaskVertex(val contents:TaskDef) extends Vertex(id=contents.name.toString(), comment=contents.comments.value)

abstract sealed class TaskSpecVertex(id:String, comment:Option[String]=None) extends Vertex(id,comment)
class TaskInputVertex(val contents:Spec, comment: Option[String]) extends TaskSpecVertex(id=contents.name, comment)
class TaskOutputVertex(val contents:Spec, comment: Option[String]) extends TaskSpecVertex(id=contents.name, comment)
class TaskParamVertex(val contents:Spec, comment: Option[String]) extends TaskSpecVertex(id=contents.name, comment) with ParamVertex

class BranchPointDefVertex(val contents:BranchPointDef) extends Vertex(id=BranchPointDef.getName(contents))

class BranchVertex(val contents:Spec) extends Vertex(id=contents.name)

sealed abstract class VariableReferenceVertex(id:String) extends Vertex(id)
//{
//  val variableName: String
//  val taskName: Option[String]
//  val branchGraftElements: Seq[BranchGraftElement]
//}

class TaskVariableVertex(val contents:TaskVariable) extends VariableReferenceVertex(id=contents.toString())
//{
//  val variableName = contents.value
//  val taskName = Some(contents.taskName)
//  val branchGraftElements = Seq[BranchGraftElement]()
//}

class ShorthandTaskVariableVertex(val contents:ShorthandTaskVariable) extends VariableReferenceVertex(id=contents.toString())
//extends VariableReferenceVertex(id="$%s@%s".format(variableName,contents.taskName)) {
//  val taskName = Some(contents.taskName)
//  val branchGraftElements = Seq[BranchGraftElement]()
//}

class ConfigVariableVertex(val contents:ConfigVariable) extends VariableReferenceVertex(id=contents.toString())

class ShorthandConfigVariableVertex(val contents:ShorthandConfigVariable) extends VariableReferenceVertex(id=contents.toString())
//extends VariableReferenceVertex(id="$%s".format(variableName)) {
//  val taskName = None
//  val branchGraftElements = Seq[BranchGraftElement]()
//}

class BranchGraftVertex(val contents:BranchGraft) extends VariableReferenceVertex(id=contents.toString())
//{
//  val variableName = contents.variableName
//  val taskName = contents.taskName
//  val branchGraftElements = contents.branchGraftElements
//}

class ShorthandBranchGraftVertex(val contents:ShorthandBranchGraft) extends VariableReferenceVertex(id=contents.toString())
//extends VariableReferenceVertex(id="$%s@%s%s".format(variableName,contents.taskName,contents.branchGraftElements.toString())) {
//  val taskName = Some(contents.taskName)
//  val branchGraftElements = contents.branchGraftElements
//}

class PlanDefinitionVertex(val contents:PlanDefinition) extends Vertex(id=contents.name.getOrElse("*anonymousPlan*"))
