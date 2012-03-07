package ducttape.syntax

import scala.util.parsing.input.Positional

object AbstractSyntaxTree {

  /** Parent class of all types representing elements in an abstract syntax tree. */
  abstract class ASTType extends Positional {}

}