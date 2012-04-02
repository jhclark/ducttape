package ducttape.syntax

import scala.util.parsing.input.Positional
import java.io.File

object AbstractSyntaxTree {

  /** Parent class of all types representing elements in an abstract syntax tree. */
  abstract class ASTType extends Positional {}

  class Comments(val value: Option[String]) extends ASTType {
    override def toString() = value match {
      case Some(s:String) => s
      case None           => ""
      }
  }
  
  /** Type of the right hand side in an assignment. */
  abstract class RValue extends ASTType;
  
  /** Unbound is the right hand side type in an underspecified variable declaration.
   *  
   *  Conceptually, Unbound can be thought of as the None case if one were to define Option[+RValue].  
   */
  case class Unbound() extends RValue {
    override def toString() = ""
  }
  
  /** Type of a literal string value, in the right-hand side context of an assignment. */
  case class Literal(val value: String) extends RValue {
    override def toString() = "Literal='%s'".format(value)
  }  
  
  /** Type of a variable reference, in the right-hand side context of an assignment. */
  case class ConfigVariable(val value: String) extends RValue {
    override def toString() = "$%s".format(value)
  }

  /** Type of a variable reference attached to a specific task, 
   * in the right-hand side context of an assignment. */
  case class TaskVariable(val taskName: String, val value: String) extends RValue {
    override def toString() = "$%s@%s".format(value,taskName)
  }  
  
  /** 
   * Type of a branch point that defines a sequence, 
   * in the right-hand side context of an assignment. 
   */
  case class SequentialBranchPoint(val branchPointName: Option[String], 
                                   val start: BigDecimal, 
                                   val end: BigDecimal,
                                   val increment: BigDecimal) extends RValue {
    override def toString = {
        branchPointName match {
          case Some(s) => "(%s: %s..%s..%s)".format(s,start,end,increment)
          case None    => "(%s..%s..%s)".format(start,end,increment)
        }
    }
  }  
  
  /** Pair containing a branch point name and a branch name. */
  class BranchGraftElement(val branchPointName:String, 
                           val branchName:String) extends ASTType {
    override def toString() = "%s:%s".format(branchPointName,branchName)
  }
  
  /** Type of a branch graft, in the right-hand side context of an assignment. */
  case class BranchGraft(val variableName:String,
                         val taskName:String,
                         val branchGraftElements:Seq[BranchGraftElement]) extends RValue {
    override def toString() = 
      "$%s@%s%s".format(variableName,taskName,branchGraftElements.toString())
  }
  
  
  /**
   * Abstract specification of a variable name and its right hand side.
   */
  class AbstractSpec[+RValue] (val name: String, val rval: RValue, val dotVariable: Boolean) extends ASTType {
    override def hashCode() = name.hashCode()
    override def equals(that: Any) = that match { case other: AbstractSpec[_] => (other.name == this.name) }
    override def toString() = "%s=%s".format(name, rval)
  } 
  
  type Spec = AbstractSpec[RValue]
  type LiteralSpec = AbstractSpec[Literal]
  
  /**
   * A key=value assignment defined in a "config" definition or config file.
   */
  class ConfigAssignment(val spec: Spec, val comments: Comments) extends ASTType {
    override def toString() = spec.toString()
  }
  
  abstract class Specs extends ASTType {
    val specs: Seq[Spec]
    val comments: Comments
  }
  
  case class TaskInputs(val specs: Seq[Spec], val comments: Comments) extends Specs;
  case class TaskOutputs(val specs: Seq[Spec], val comments: Comments) extends Specs;  
  case class TaskParams(val specs: Seq[Spec], val comments: Comments) extends Specs;
  case class TaskPackageNames(val specs: Seq[Spec], val comments: Comments) extends Specs;
  
 /** Branch in a hyperworkflow, defined in the right hand side of a variable declaration. */
  case class BranchPointDef(val name: Option[String], val specs: Seq[Spec]) extends RValue {
    override def toString() = {
      name match {
        case Some(s) => "(%s: %s)".format(s, specs.mkString(" "))
        case None    => "(%s)".format(specs.mkString(" "))
      } 
    }
  }  

 /** Reference, in a plan, to a branchpoint and one or more of its branches. */
  case class BranchPointRef(val name: String, val branchNames: Seq[String]) extends ASTType {
    override def toString() = {
      "(%s: %s)".format(name, branchNames.mkString(" "))
    }
  }    
  
  class ShellCommands(val value: String) extends ASTType {
    override def toString() = value
  }
  
//  class PackageNames(val comments:Comments, val packageNames: List[String]) extends Specs {
//    override def toString = comments.toString() + "\n" + List(packageNames).mkString(" ")
//  }
  
  class TaskHeader(val specsList: List[Specs]) extends ASTType {
    override def toString() = specsList.mkString(" ")
  } 
  
  /** Defines a block of ducttape code, such as a task definition. */
  class Block extends ASTType;
  
  /**
   * Short for "TaskDefinition". Abbreviated due to its pervasiveness in the code.
   *
   * @param keyword The keyword used to declare this taskdef. Usually (always?) "task".
   */
  class TaskDef(val comments: Comments,
                val keyword: String,
                val name: String, 
                val header: TaskHeader, 
                val commands: ShellCommands) extends Block {

    lazy val packageSpecList: Seq[Specs] = header.specsList.filter{ specs: Specs => specs.isInstanceOf[TaskPackageNames] }
    lazy val inputSpecList: Seq[Specs] = header.specsList.filter{ specs: Specs => specs.isInstanceOf[TaskInputs] }
    lazy val outputSpecList: Seq[Specs] = header.specsList.filter{ specs: Specs => specs.isInstanceOf[TaskOutputs] }
    lazy val paramSpecList: Seq[Specs] = header.specsList.filter{ specs: Specs => specs.isInstanceOf[TaskParams] }
    
    // a few convenience methods:
    lazy val packages: Seq[Spec] = packageSpecList.flatMap{_.specs}
    lazy val inputs: Seq[Spec] = inputSpecList.flatMap{_.specs}
    lazy val outputs: Seq[Spec] = outputSpecList.flatMap{_.specs}
    lazy val params: Seq[Spec] = paramSpecList.flatMap{_.specs}
    
    lazy val lastHeaderLine: Int = {
      val n = header.specsList.flatMap{specs: Specs => specs.specs.map{spec: Spec => spec.pos.line}}.max
      System.err.println("lazy header line... " + n)
      n
    }
    
    override def toString = name
  }

  class CallDefinition(val comments:Comments,
                       val name: String, 
                       val header:TaskHeader, 
                       val functionName:String) extends Block {
    override def toString() = name
  }
  
  class GroupDefinition(val comments:Comments,
                        val keyword: String,
                        val name: String, 
                        val header: TaskHeader,
                        val blocks: Seq[Block]) extends Block {
    override def toString() = name
  }
  
  class ConfigDefinition(val comments: Comments,
                         val name: Option[String],
                         val lines: Seq[ConfigAssignment]) extends Block {
    override def toString() = {
      name match {
        case None => "GLOBAL"
        case Some(s:String) => s
      }
    }
  }
  
  class CrossProduct(val goals: Seq[String], val value: Seq[BranchPointRef]) extends ASTType {
    override def toString = {
      "reach %s via %s".format(goals.mkString(","),value.mkString(" * ")) 
    }
  }
  
  class PlanDefinition(val comments: Comments,
                       val name: Option[String],
                       val crossProducts: Seq[CrossProduct]) extends Block {
    override def toString() = {
      name match {
        case None => "GLOBAL"
        case Some(s:String) => s
      }
    }
  }
  
  /** Ducttape hyperworkflow file. */
  class WorkflowDefinition(val file: File, val blocks: Seq[Block]) extends ASTType {
    lazy val tasks: Seq[TaskDef] = blocks.filter(_.isInstanceOf[TaskDef]).map(_.asInstanceOf[TaskDef])
    
    override def toString() = blocks.mkString("\n\n")
  }  
  
}