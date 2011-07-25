package ducttape.syntax;

import scala.util.parsing.input.Positional


class CommentBlock(val comments: Seq[String]) extends Positional {
	override def toString = comments.mkString("\n")
}

abstract class RValue;
case class Unbound extends RValue {
	override def toString = ""
}
case class Literal(value: String) extends RValue {
	override def toString = value
}
case class Variable(task: String, value: String) extends RValue {
	override def toString = "$" + task + "/" + value
}

class TaskHeader(val name: String) {
	override def toString = name; 
}


