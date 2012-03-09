package ducttape.syntax

import scala.util.parsing.input.Positional
import java.io.File

object AbstractSyntaxTree {

  /** Parent class of all types representing elements in an abstract syntax tree. */
  abstract class ASTType extends Positional {}

  
  
  /** Type of the right hand side in an assignment. */
  abstract class RValue extends ASTType;
  
  /** Type of a literal string value in the right-hand side context of an assignment. */
  case class Literal(value: String) extends RValue {
    override def toString = "Literal='%s'".format(value)
  }  
  
  /** Type of a variable reference in the right-hand side context of an assignment. */
  case class VariableReference(value: String) extends RValue {
    override def toString = "$%s".format(value)
  }
  
  /** Pair containing a branch point name and a branch name. */
  class BranchGraftElement(branchPointName:String,branchName:String) extends ASTType {
    override def toString = "%s:%s".format(branchPointName,branchName)
  }
  
  /** Type of a branch graft in the right-hand side context of an assignment. */
  case class BranchGraft(variableName:String,
                    taskName:String,
                    branchGraftElements:Seq[BranchGraftElement]) extends RValue {
    override def toString = 
      "$%s@%s%s".format(variableName,taskName,branchGraftElements.toString())
  }
  
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