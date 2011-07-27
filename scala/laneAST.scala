package ducttape.syntax;

import scala.util.parsing.input.Positional

object AbstractSyntaxTree {
  
/** Parent class of all types representing elements in an abstract syntax tree. */
abstract class ASTType extends Positional {}

/** Block of comments. */
class CommentBlock(val comments: Seq[String]) extends ASTType {
	override def toString = comments.toString//comments.mkString("\n");
}


/**
 * Abstract specification of a variable name and its right hand side.
 */
class AbstractSpec[+RValue] (val name: String, val rval: RValue) extends ASTType {
  override def toString = name + "=" + rval;
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
	override def toString = "";
}

/** Type of a literal string value right hand side in a variable declaration. */
case class Literal(value: String) extends RValue {
	override def toString = value;
}

/** Type of a variable reference right hand side in a variable declaration. */
case class Variable(task: String, value: String) extends RValue {
	override def toString = "$" + task + "/" + value;
}

class TaskHeader(val name: String, val inputs: Seq[Spec], val outputs: Seq[Spec], val params: Seq[Spec]) extends ASTType {
	override def toString = name + " " + " " + inputs + " " + outputs + " " + params;
}

class TaskDef(val name: String,
              val comments: CommentBlock,
              val inputs: Seq[Spec],
              val outputs: Seq[Spec],
              val params: Seq[Spec],
              val commands: Seq[String]) {
  override def toString = name + " " + comments.toString + " " + inputs + " " + outputs + " " + commands
}

}