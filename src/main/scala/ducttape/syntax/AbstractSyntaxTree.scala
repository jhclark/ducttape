package ducttape.syntax

import scala.util.parsing.input.Positional
import java.io.File

object AbstractSyntaxTree {

  /** Parent class of all types representing elements in an abstract syntax tree. */
  abstract class ASTType extends Positional {}

  /** Ducttape file. */
  class Tape(val tasks: Seq[Block]) extends ASTType {
    override def toString = tasks.mkString("\n\n")
  }
  
  /** Defines a block of ducttape code, such as a task definition. */
  class Block extends ASTType
  
  class TaskDefinition(val name: String) extends Block {
    override def toString = name
  }
  
}