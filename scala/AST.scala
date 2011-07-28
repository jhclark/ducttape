package ducttape.syntax;

import scala.util.parsing.input.Positional

object AbstractSyntaxTree {
  
  /** Parent class of all types representing elements in an abstract syntax tree. */
  abstract class ASTType extends Positional {}

  /** Ducttape hyperworkflow file. */
  class WorkflowDefinition(val tasks: Seq[TaskDef]) extends ASTType {
    override def toString = tasks.mkString("\n\n")
  }

  /** Block of comments. */
  class CommentBlock(val comments: Seq[String]) extends ASTType {
    override def toString = comments.toString //comments.mkString("\n");
  }


  /**
   * Abstract specification of a variable name and its right hand side.
   */
  class AbstractSpec[+RValue] (val name: String, val rval: RValue) extends ASTType {
    override def toString = "%s=%s".format(name, rval)
  }

  type Spec = AbstractSpec[RValue]
  
  type LiteralSpec = AbstractSpec[Literal]


  /** Right hand side type in a variable declaration. */
  abstract class RValue extends ASTType;

  /** Unbound is the right hand side type in an underspecified variable declaration.
   *  
   *  Conceptually, Unbound can be thought of as the None case if one were to define Option[+RValue].  
   */
  case class Unbound extends RValue {
    override def toString = ""
  }

  /** Type of a literal string value right hand side in a variable declaration. */
  case class Literal(value: String) extends RValue {
    override def toString = "'%s'".format(value)
  }

  /** Type of a variable reference right hand side in a variable declaration. */
  case class Variable(task: String, value: String) extends RValue {
    override def toString = "${%s}/%s".format(task,value)
  }

  /** Branch in a hyperworkflow, defined in the right hand side of a variable declaration. */
  case class Branch(val name: String, val specs: Seq[Spec]) extends RValue {
    override def toString = List(name, specs.toString).mkString(" ")
  }

  class TaskHeader(val name: String,
                   val inputs: Seq[Spec],
                   val outputs: Seq[Spec],
                   val params: Seq[Spec]) extends ASTType {
    override def toString = List(name, inputs, outputs, params).mkString(" ")
  }
  
  class TaskDef(val name: String,
                val comments: CommentBlock,
                val inputs: Seq[Spec],
                val outputs: Seq[Spec],
                val params: Seq[Spec],
                val commands: Seq[String]) {
    override def toString =
      "Task name: " + name + 
    "\n comments: " + comments.toString + 
    "\n   inputs: " + inputs + 
    "\n  outputs: " + outputs + 
    "\n   params: " + params + 
    "\n commands: " + commands
  }
}
