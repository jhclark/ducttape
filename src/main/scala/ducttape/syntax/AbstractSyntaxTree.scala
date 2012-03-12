package ducttape.syntax

import scala.util.parsing.input.Positional
import java.io.File

object AbstractSyntaxTree {

  /** Parent class of all types representing elements in an abstract syntax tree. */
  abstract class ASTType extends Positional {}

  
  
  /** Type of the right hand side in an assignment. */
  abstract class RValue extends ASTType;
  
  /** Unbound is the right hand side type in an underspecified variable declaration.
   *  
   *  Conceptually, Unbound can be thought of as the None case if one were to define Option[+RValue].  
   */
  case class Unbound() extends RValue {
    override def toString = ""
  }
  
  /** Type of a literal string value, in the right-hand side context of an assignment. */
  case class Literal(val value: String) extends RValue {
    override def toString = "Literal='%s'".format(value)
  }  
  
  /** Type of a variable reference, in the right-hand side context of an assignment. */
  case class Variable(val value: String) extends RValue {
    override def toString = "$%s".format(value)
  }

  /** Type of a variable reference attached to a specific task, 
   * in the right-hand side context of an assignment. */
  case class TaskVariable(val taskName:String, val value: String) extends RValue {
    override def toString = "$%s@%s".format(value,taskName)
  }  
  
  /** 
   * Type of a branch point that defines a sequence, 
   * in the right-hand side context of an assignment. 
   */
  case class SequentialBranchPoint(val branchPointName:String, 
                                   val start:BigDecimal, 
                                   val end:BigDecimal,
                                   val increment:BigDecimal) extends RValue {
    override def toString = "(%s: %d..%d..%d".format(branchPointName,start,end,increment)
  }  
  
  /** Pair containing a branch point name and a branch name. */
  class BranchGraftElement(val branchPointName:String, 
                           val branchName:String) extends ASTType {
    override def toString = "%s:%s".format(branchPointName,branchName)
  }
  
  /** Type of a branch graft, in the right-hand side context of an assignment. */
  case class BranchGraft(val variableName:String,
                         val taskName:String,
                         val branchGraftElements:Seq[BranchGraftElement]) extends RValue {
    override def toString = 
      "$%s@%s%s".format(variableName,taskName,branchGraftElements.toString())
  }
  
  
  /**
   * Abstract specification of a variable name and its right hand side.
   */
  class AbstractSpec[+RValue] (val name: String, val rval: RValue) extends ASTType {
    override def hashCode = name.hashCode
    override def equals(that: Any) = that match { case other: AbstractSpec[_] => (other.name == this.name) }
    override def toString = "%s=%s".format(name, rval)
  } 
  
  type Spec = AbstractSpec[RValue]
    
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