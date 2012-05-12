package ducttape.syntax

import scala.util.parsing.input.Positional
import java.io.File
import scala.util.parsing.input.Position
import scala.collection.immutable.NumericRange.Inclusive

object AbstractSyntaxTree {

  /** Parent class of all types representing elements in an abstract syntax tree. */
  trait ASTType extends Positional {
    private var _file = new File("unknown_file")
    def declaringFile_=(f: File) { _file = f }
    def declaringFile: File = _file
    
    // can be overridden to give better error messages (e.g. TaskHeaders may cover a large span)
    def endPos: Position = pos
    
    // used for traversing AST... abstractly
    def children: Seq[ASTType]
  }

  class Comments(val value: Option[String]) extends ASTType {
    override def children = Nil
    override def toString() = value match {
      case Some(s:String) => s
      case None           => ""
    }
  }
  
  /** Type of the right hand side in an assignment. */
  trait RValue extends ASTType;
  
  /** Unbound is the right hand side type in an underspecified variable declaration.
   *  
   *  Conceptually, Unbound can be thought of as the None case if one were to define Option[+RValue].  
   */
  case class Unbound() extends RValue {
    override def children = Nil
    override def toString() = ""
  }
  
  /** Type of a literal string value, in the right-hand side context of an assignment. */
  case class Literal(val value: String) extends RValue {
    override def children = Nil
    override def toString() = "'%s'".format(value)
  }  
  
  /** Type of a variable reference, in the right-hand side context of an assignment. */
  case class ConfigVariable(val value: String) extends RValue {
    override def children = Nil
    override def toString() = "$%s".format(value)
  }

  /** Type of a variable reference attached to a specific task, 
   * in the right-hand side context of an assignment. */
  case class TaskVariable(val taskName: String, val value: String) extends RValue {
    override def children = Nil
    override def toString() = "$%s@%s".format(value,taskName)
  }  
  
  case class Sequence(val start: BigDecimal, 
                      val end: BigDecimal,
                      val increment: BigDecimal) extends ASTType {
    override def children = Nil
    override def toString() = "%s..%s..%s".format(start,end,increment)
  }
  
  /** 
   * Type of a branch point that defines a sequence, 
   * in the right-hand side context of an assignment. 
   */
  case class SequentialBranchPoint(val branchPointName: Option[String], 
                                   val sequence: Sequence) extends RValue {
    override def children = Seq(sequence)
    override def toString() = {
        branchPointName match {
          case Some(s) => "(%s: %s)".format(s,sequence)
          case None    => "(%s)".format(sequence)
        }
    }
  }  
  
  /** Pair containing a branch point name and a branch name. */
  class BranchGraftElement(val branchPointName: String, 
                           val branchName: String) extends ASTType {
    override def children = Nil
    override def toString() = "%s:%s".format(branchPointName,branchName)
  }
  
  /** Type of a branch graft, in the right-hand side context of an assignment. */
  case class BranchGraft(val variableName: String,
                         val taskName: Option[String],
                         val branchGraftElements: Seq[BranchGraftElement]) extends RValue {
    override def children = branchGraftElements
    override def toString() = {
      taskName match {
        case Some(taskName) => "$%s@%s%s".format(variableName,taskName,branchGraftElements.toString())
        case None           => "$%s%s".format(variableName,branchGraftElements.toString())
      }
    }
  }
  
  
  /**
   * Abstract specification of a variable name and its right hand side.
   */
  class AbstractSpec[+A <: RValue](val name: String, val rval: A, val dotVariable: Boolean) extends ASTType {
    override def children = Seq(rval)
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
    override def children = Seq(spec, comments)
    override def toString() = spec.toString()
  }
  
  trait Specs extends ASTType {
    val specs: Seq[Spec]
    val comments: Comments
  }
  
  case class TaskInputs(val specs: Seq[Spec], val comments: Comments) extends Specs {
    override def children = specs ++ Seq(comments)
  }
  case class TaskOutputs(val specs: Seq[Spec], val comments: Comments) extends Specs {
    override def children = specs ++ Seq(comments)
  }
  case class TaskParams(val specs: Seq[Spec], val comments: Comments) extends Specs {
    override def children = specs ++ Seq(comments)
  }
  case class TaskPackageNames(val specs: Seq[Spec], val comments: Comments) extends Specs {
    override def children = specs ++ Seq(comments)
  }
  
 /** Branch in a hyperworkflow, defined in the right hand side of a variable declaration. */
  case class BranchPointDef(val name: Option[String], val specs: Seq[Spec]) extends RValue {
    override def children = specs
    override def toString() = {
      name match {
        case Some(s) => "(%s: %s)".format(s, specs.mkString(" "))
        case None    => "(%s)".format(specs.mkString(" "))
      } 
    }
  }  

 /** Reference, in a plan, to a branchpoint and one or more of its branches. */
  case class BranchPointRef(val name: String, val branchNames: Seq[ASTType]) extends ASTType {
    override def children = Nil
    override def toString() = {
      "(%s: %s)".format(name, branchNames.mkString(" "))
    }
  }    
  
  // NOTE: This has been replaced by the bash parser and BashCode
  class ShellCommands(val value: String) extends ASTType {
    override def children = Nil
    override def toString() = value
  }
  
//  class PackageNames(val comments:Comments, val packageNames: List[String]) extends Specs {
//    override def toString = comments.toString() + "\n" + List(packageNames).mkString(" ")
//  }
  
  class TaskHeader(val specsList: List[Specs]) extends ASTType {
    override def children = specsList
    override def toString() = specsList.mkString(" ")
  } 
  
  /** Defines a block of ducttape code, such as a task definition. */
  trait Block extends ASTType;
  
  /**
   * Short for "TaskDefinition". Abbreviated due to its pervasiveness in the code.
   *
   * @param keyword The keyword used to declare this taskdef. Usually (always?) "task".
   */
  class TaskDef(val comments: Comments,
                val keyword: String,
                val name: String, 
                val header: TaskHeader, 
                val commands: BashCode) extends Block {
    
    override def children = Seq(comments, header, commands)

    private lazy val packageSpecList: Seq[TaskPackageNames] = header.specsList.collect{ case x: TaskPackageNames => x }
    private lazy val inputSpecList: Seq[TaskInputs] = header.specsList.collect{ case x: TaskInputs => x }
    private lazy val outputSpecList: Seq[TaskOutputs] = header.specsList.collect{ case x: TaskOutputs => x }
    private lazy val paramSpecList: Seq[TaskParams] = header.specsList.collect{ case x: TaskParams => x }
    
    // a few convenience methods:
    lazy val packages: Seq[Spec] = packageSpecList.flatMap{_.specs}
    lazy val inputs: Seq[Spec] = inputSpecList.flatMap{_.specs}
    lazy val outputs: Seq[Spec] = outputSpecList.flatMap{_.specs}
    lazy val params: Seq[Spec] = paramSpecList.flatMap{_.specs}
    
    override lazy val endPos: Position = {
      val specs: Seq[Spec] = header.specsList.flatMap{ specs: Specs => specs.specs }
      // TODO: Define ordering on positional so that we can find last column, too
      val lastSpec: Spec = specs.maxBy[Int]{spec: Spec => spec.pos.line}
      lastSpec.pos
    }
    
    override def hashCode() = name.hashCode()
    override def equals(that: Any) = that match { case other: TaskDef => (other.name == this.name) }
    override def toString() = name
  }
  type PackageDef = TaskDef
  type ActionDef = TaskDef

  class CallDefinition(val comments: Comments,
                       val name: String, 
                       val header: TaskHeader, 
                       val functionName: String) extends Block {
    override def children = Seq(comments, header)
    override def toString() = name
  }
  
  class GroupDefinition(val comments: Comments,
                        val keyword: String,
                        val name: String, 
                        val header: TaskHeader,
                        val blocks: Seq[Block]) extends Block {
    private lazy val taskLikes = blocks.collect{ case x: ActionDef => x}
    
    // only has members for SubmitterDefs (but adding more info to the typesystem gets ridiculous)
    lazy val actions: Seq[ActionDef] = taskLikes.filter(_.keyword == "action")
    
    override def children = Seq(comments, header) ++ blocks
    override def toString() = name
  }
  type VersionerDef = GroupDefinition
  type SubmitterDef = GroupDefinition
  
  // TODO: use the Pimp My Library Pattern to add certain methods to certain keywords?
  
  class ConfigDefinition(val keyword: String,
                         val comments: Comments,
                         val name: Option[String],
                         val lines: Seq[ConfigAssignment]) extends Block {
    override def children = Seq(comments) ++ lines
    override def toString() = {
      name match {
        case None => "GLOBAL"
        case Some(s: String) => s
      }
    }
  }
  
  class CrossProduct(val goals: Seq[String], val value: Seq[BranchPointRef]) extends ASTType {
    override def children = value
    override def toString() = {
      "reach %s via %s".format(goals.mkString(","),value.mkString(" * ")) 
    }
  }
  
  class PlanDefinition(val comments: Comments,
                       val name: Option[String],
                       val crossProducts: Seq[CrossProduct]) extends Block {
    override def children = Seq(comments) ++ crossProducts
    override def toString() = name match {
      case None => "GLOBAL"
      case Some(s: String) => s
    }
  }
  
  /** Ducttape hyperworkflow file. */
  class WorkflowDefinition(val blocks: Seq[Block]) extends ASTType {
    override def children = blocks
    
    lazy val plans: Seq[PlanDefinition] = blocks.collect{case x: PlanDefinition => x}
    
    private lazy val groupLikes: Seq[GroupDefinition] = blocks.collect{case x: GroupDefinition => x}
    lazy val versioners: Seq[VersionerDef] = groupLikes.filter(_.keyword == "versioner")
    lazy val submitters: Seq[SubmitterDef] = groupLikes.filter(_.keyword == "submitter")
    
    private lazy val configLikes: Seq[ConfigDefinition] = blocks.collect{case x: ConfigDefinition => x}
    lazy val configs: Seq[ConfigDefinition] = configLikes.filter{t: ConfigDefinition => t.keyword == "config"}
    lazy val globals: Seq[ConfigAssignment] = configLikes.filter{t: ConfigDefinition => t.keyword == "global"}.flatMap{ _.lines}
    
    private lazy val taskDefs: Seq[TaskDef] = blocks.collect{case x: TaskDef => x}
    lazy val tasks: Seq[TaskDef] = taskDefs.filter{t: TaskDef => t.keyword == "task"}
    lazy val packages: Seq[TaskDef] = taskDefs.filter{t: TaskDef => t.keyword == "package"}
    
    override def toString() = blocks.mkString("\n\n")
  }  
  
}
