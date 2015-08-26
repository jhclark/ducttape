// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax

import scala.util.parsing.input.Positional
import java.io.File
import scala.util.parsing.input.Position
import scala.collection.immutable.NumericRange.Inclusive

import ducttape.util.Anonymous

object AbstractSyntaxTree {

  object ASTType {
    val UnknownFile = new File("unknown_file")
  }

  /** Parent class of all types representing elements in an abstract syntax tree. */
  sealed trait ASTType extends Positional {
    private var _file = ASTType.UnknownFile
    def declaringFile_=(f: File) { _file = f }
    def declaringFile: File = _file

    // can be overridden to give better error messages (e.g. TaskHeaders may cover a large span)
    def endPos: Position = pos

    // used for traversing AST... abstractly
    def children: Seq[ASTType]
  }

  case class Comments(val value: Option[String]) extends ASTType {
    override def children = Nil
    override def toString() = value match {
      case Some(s:String) => s
      case None           => ""
    }
  }

  /** Type of the right hand side in an assignment. */
  sealed trait RValue extends ASTType;

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

  /** Type of a shorthand variable reference attached to a specific task,
   * in the right-hand side context of an assignment. */
  case class ShorthandTaskVariable(val taskName: String) extends RValue {
    override def children = Nil
    override def toString() = "@%s".format(taskName)
  }

  /** Type of a shorthand global or config variable reference,
   * in the right-hand side context of an assignment. */
  case class ShorthandConfigVariable() extends RValue {
    override def children = Nil
    override def toString() = "@"
  }

  case class Sequence(val start: BigDecimal,
                      val end: BigDecimal,
                      val increment: BigDecimal) extends ASTType {
    override lazy val children = {
      this.toSeq.map({ number:BigDecimal => {
        val string = number.toString()
        new BranchSpec(string, new Literal(string))
      }})
    }

    override def toString() = "%s..%s..%s".format(start,end,increment)

    private lazy val toSeq : Seq[BigDecimal] = {
      new scala.collection.mutable.ArrayBuffer[BigDecimal] {
        var currentValue = start
        while (currentValue <= end) {
          append(currentValue)
          currentValue += increment
        }
      }
    }.toSeq
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

  object SequentialBranchPoint {

    private val map = new java.util.IdentityHashMap[SequentialBranchPoint,String]

    def getName(branchPointDef:SequentialBranchPoint) : String = {
      return branchPointDef.branchPointName match {
        case Some(name) => name
        case None       => {
          if (map.containsKey(branchPointDef)) {
            map.get(branchPointDef)
          } else {
            val name = "*anonymousBranchPoint%s*)".format(map.size)
            map.put(branchPointDef, name)
            name
          }
        }
      }
    }

  }

  /** Pair containing a branch point name and a branch name. */
  case class BranchGraftElement(val branchPointName: String,
                           val branchName: String) extends ASTType {
    override def children = Nil
    override def toString() = "%s:%s".format(branchPointName,branchName)
    lazy val isGlob = branchName=="*"
  }

  /** Type of a branch graft, in the right-hand side context of an assignment. */
  case class BranchGraft(val variableName: String,
                         val taskName: Option[String],
                         val branchGraftElements: Seq[BranchGraftElement]) extends RValue {
    override def children = branchGraftElements

    def toString(withBranchGraftElements:Boolean) : String = {
      val branchGraftString = if (withBranchGraftElements) branchGraftElements.mkString("[", ",", "]") else ""

      return taskName match {
        case Some(taskName) => "$%s@%s%s".format(variableName,taskName,branchGraftString)
        case None           => "$%s%s".format(variableName,branchGraftString)
      }
    }

    override def toString() = toString(withBranchGraftElements=true)
  }

  /** Type of a shorthand branch graft, in the right-hand side context of an assignment. */
  case class ShorthandBranchGraft(val taskName: String,
                         val branchGraftElements: Seq[BranchGraftElement]) extends RValue {
    override def children = branchGraftElements
    override def toString() = {
      "@%s%s".format(taskName,branchGraftElements.toString())
    }
  }


  /**
   * Abstract specification of a variable name and its right hand side.
   */
  abstract sealed class AbstractSpec[+A <: RValue] extends ASTType {
    val name: String
    val rval: A
    val dotVariable: Boolean
    override def children = Seq(rval)
    override def hashCode() = name.hashCode()
    override def equals(that: Any) = that match { case other: AbstractSpec[_] => (other.name == this.name) }
    override def toString() = {
      val displayName = if (dotVariable) "." + name else name
      "%s=%s".format(displayName, rval)
    }
  }
  type Spec = AbstractSpec[RValue]
  type LiteralSpec = AbstractSpec[Literal]

  case class ConfigParamSpec[+A <: RValue](val name:String, val rval:A, val dotVariable:Boolean) extends AbstractSpec[A] //{System.out.println("ConfigParamSpec=" + name)}

  case class TaskParamSpec[+A <: RValue](val name:String, val rval:A, val dotVariable:Boolean) extends AbstractSpec[A] //{System.out.println("TaskParamSpec=" + name)}

  case class TaskInputSpec[+A <: RValue](val name:String, val rval:A) extends AbstractSpec[A] {
    val dotVariable:Boolean = false
  }

  case class TaskOutputSpec[+A <: RValue](val name:String, val rval:A) extends AbstractSpec[A] {
    val dotVariable:Boolean = false
  }

  case class PackageSpec(val name:String) extends AbstractSpec[Unbound] {
    val rval = Unbound()
    val dotVariable:Boolean = false
  }

  case class BranchSpec[+A <: RValue](val name:String, val rval:A) extends AbstractSpec[A] {
    val dotVariable:Boolean = false
  }


  /**
   * A key=value assignment defined in a "config" definition or config file.
   */
  case class ConfigAssignment(val spec: Spec, val comments: Comments) extends ASTType {
    override def children = Seq(spec, comments)
    override def toString() = spec.toString()
  }

  sealed trait Specs extends ASTType {
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

  object BranchPointDef {

    private val map = new java.util.IdentityHashMap[BranchPointDef,String]

    def getName(branchPointDef:BranchPointDef) : String = {
      return branchPointDef.name match {
        case Some(name) => name
        case None       => {
          if (map.containsKey(branchPointDef)) {
            map.get(branchPointDef)
          } else {
            val name = "*anonymousBranchPoint%s*".format(map.size)
            map.put(branchPointDef, name)
            name
          }
        }
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

  // TODO: Pass a StringBuilder down through the AST to make stringification faster
  case class BashCode(val code: String, val vars: Set[String] = Set.empty) extends ASTType {
    override def children = Nil // TODO: Name exactly what line vars come from
    override def toString = code
  }

//  case class PackageNames(val comments:Comments, val packageNames: List[String]) extends Specs {
//    override def toString = comments.toString() + "\n" + List(packageNames).mkString(" ")
//  }

  case class TaskHeader(val specsList: List[Specs]) extends ASTType {
    override def children = specsList
    override def toString() = specsList.mkString(" ")
  }

  /** Defines a block of ducttape code, such as a task definition. */
  sealed trait Block extends ASTType {
    val comments: Comments
  }

  /**
   * A task-like block.
   *
   * @param keyword The keyword used to declare this block.
   */
  abstract sealed case class TaskLike(val comments: Comments,
                val keyword: String,
                val name: Namespace, // TODO: Rename name to namespace to prevent namespace == name confusion
                val header: TaskHeader,
                val commands: BashCode) extends Block {

    override def children = Seq(comments, header, commands)

    private def packageSpecList: Seq[TaskPackageNames] = header.specsList.collect{ case x: TaskPackageNames => x }
    private def inputSpecList: Seq[TaskInputs] = header.specsList.collect{ case x: TaskInputs => x }
    private def outputSpecList: Seq[TaskOutputs] = header.specsList.collect{ case x: TaskOutputs => x }
    private def paramSpecList: Seq[TaskParams] = header.specsList.collect{ case x: TaskParams => x }

    // a few convenience methods:
    lazy val packages: Seq[Spec] = packageSpecList.flatMap(_.specs)
    lazy val inputs: Seq[Spec] = inputSpecList.flatMap(_.specs)
    lazy val outputs: Seq[Spec] = outputSpecList.flatMap(_.specs)
    lazy val params: Seq[Spec] = paramSpecList.flatMap(_.specs)
    lazy val allSpecs: Seq[Spec] = header.specsList.flatMap { specs: Specs => specs.specs }

    override lazy val endPos: Position = {
      // TODO: Define ordering on positional so that we can find last column, too
      if (allSpecs.size == 0) {
        header.pos
      } else {
        val lastSpec: Spec = allSpecs.maxBy[Int] { spec: Spec => spec.pos.line }
        lastSpec.pos
      }
    }

    override def hashCode() = name.hashCode()
    override def equals(that: Any) = that match { case other: TaskLike => (other.name == this.name) }
    override def toString() = name.toString
  }

  class ActionDef(       comments: Comments, name: Namespace, header:TaskHeader, commands:BashCode) extends TaskLike(comments, keyword="action",   name, header, commands)
  class BaselineBlockDef(comments: Comments, name: Namespace, header:TaskHeader, commands:BashCode) extends TaskLike(comments, keyword="baseline", name, header, commands)
  class BranchBlockDef(  comments: Comments, name: Namespace, header:TaskHeader, commands:BashCode) extends TaskLike(comments, keyword="branch",   name, header, commands)
  class FuncDef(         comments: Comments, name: Namespace, header:TaskHeader, commands:BashCode) extends TaskLike(comments, keyword="func",     name, header, commands)
  class PackageDef(      comments: Comments, name: Namespace, header:TaskHeader, commands:BashCode) extends TaskLike(comments, keyword="package",  name, header, commands)
  class SummaryOfDef(    comments: Comments, name: Namespace, header:TaskHeader, commands:BashCode) extends TaskLike(comments, keyword="of",       name, header, commands)

  /** Short for "TaskDefinition". Abbreviated due to its pervasiveness in the code. */
  class TaskDef(         comments: Comments, name: Namespace, header:TaskHeader, commands:BashCode) extends TaskLike(comments, keyword="task",     name, header, commands) {
    /**
     * Constructs a concrete task definition
     * from a function definition and a function call.
     */
    def this(taskName: Namespace, functionDefinition: FuncDef, functionCall: CallDefinition) =
      this(functionCall.comments,
           taskName,
           functionCall.header,
           functionDefinition.commands)
  }

  case class CallDefinition(val comments: Comments,
                       val name: String,
                       val header: TaskHeader,
                       val functionName: Namespace) extends Block {
    override def children = Seq(comments, header)
    override def toString() = name
  }

  abstract sealed case class GroupLike(val comments: Comments,
                        val keyword: String,
                        val name: Namespace,
                        val header: TaskHeader,
                        val blocks: Seq[Block]) extends Block {
    private lazy val taskLikes = blocks.collect{ case x: TaskLike => x}

    // only has members for SubmitterDefs and VersionerDefs (but adding more info to the typesystem gets ridiculous)
    lazy val actions: Seq[ActionDef] = taskLikes.collect{ case x: ActionDef => x } //taskLikes.filter(_.keyword == "action")

    // only has members for SummaryDefs (but adding more info to the typesystem gets ridiculous)
    lazy val ofs: Seq[SummaryOfDef] = taskLikes.collect{ case x: SummaryOfDef => x } //taskLikes.filter(_.keyword == "of")

    private lazy val packageSpecList: Seq[TaskPackageNames] = header.specsList.collect { case x: TaskPackageNames => x }
    private lazy val inputSpecList: Seq[TaskInputs] = header.specsList.collect { case x: TaskInputs => x }
    private lazy val outputSpecList: Seq[TaskOutputs] = header.specsList.collect { case x: TaskOutputs => x }
    private lazy val paramSpecList: Seq[TaskParams] = header.specsList.collect { case x: TaskParams => x }

    // a few convenience methods:
    lazy val packages: Seq[Spec] = packageSpecList.flatMap(_.specs)
    lazy val inputs: Seq[Spec] = inputSpecList.flatMap(_.specs)
    lazy val outputs: Seq[Spec] = outputSpecList.flatMap(_.specs)
    lazy val params: Seq[Spec] = paramSpecList.flatMap(_.specs)
    lazy val allSpecs: Seq[Spec] = header.specsList.flatMap { specs: Specs => specs.specs }

    override def children = Seq(comments, header) ++ blocks
    override def toString() = name.toString
  }

  class BranchPointBlock(comments: Comments, name: Namespace, header:TaskHeader, blocks:Seq[Block]) extends GroupLike(comments, keyword="branchpoint", name, header, blocks)
  class GroupDef(        comments: Comments, name: Namespace, header:TaskHeader, blocks:Seq[Block]) extends GroupLike(comments, keyword="group",       name, header, blocks)
  class SubmitterDef(    comments: Comments, name: Namespace, header:TaskHeader, blocks:Seq[Block]) extends GroupLike(comments, keyword="submitter",   name, header, blocks)
  class SummaryDef(      comments: Comments, name: Namespace, header:TaskHeader, blocks:Seq[Block]) extends GroupLike(comments, keyword="summary",     name, header, blocks)
  class VersionerDef(    comments: Comments, name: Namespace, header:TaskHeader, blocks:Seq[Block]) extends GroupLike(comments, keyword="versioner",   name, header, blocks)


  // TODO: use the Pimp My Library Pattern to add certain methods to certain keywords?

  case class ConfigDefinition(val keyword: String,
                         val comments: Comments,
                         val name: Option[String],
                         val lines: Seq[ConfigAssignment]) extends Block {
    override def children = Seq(comments) ++ lines
    override def toString() = {
      name match {
        case None => ConfigDefinition.getName(this)
        case Some(s: String) => s
      }
    }
  }

  object ConfigDefinition extends Anonymous[ConfigDefinition] {
    def anonymousString(n:Int) = "*%s%s*".format("anonymousConfig", n)
    def lookupName(config:ConfigDefinition) = config.name
  }


  case class CrossProduct(val goals: Seq[String], val value: Seq[BranchPointRef]) extends ASTType {
    override def children = value
    override def toString() = {
      "reach %s via %s".format(goals.mkString(","),value.mkString(" * "))
    }
  }

  object CrossProduct extends Anonymous[CrossProduct] {
    def anonymousString(n:Int) = "*%s%s*".format("crossProduct", n)
    def lookupName(plan:CrossProduct) = None
  }

  case class PlanDefinition(val comments: Comments,
                       val name: Option[String],
                       val crossProducts: Seq[CrossProduct]) extends Block {
    override def children = Seq(comments) ++ crossProducts
    override def toString() = name match {
      case None => PlanDefinition.getName(this)
      case Some(s: String) => s
    }
  }

  object PlanDefinition extends Anonymous[PlanDefinition] {
    def anonymousString(n:Int) = "*%s%s*".format("anonymousPlan", n)
    def lookupName(plan:PlanDefinition) = plan.name
  }

  /** Ducttape hyperworkflow file. */
  case class WorkflowDefinition(val elements: Seq[ASTType],
                           val files: Seq[File], // what files is this workflow definition composed of?
                           private val hadImports: Boolean = false,
                           private val isImported: Boolean = false) extends ASTType {
    override def children = elements

    lazy val blocks: Seq[Block] = elements.collect { case x: Block => x }

    lazy val plans: Seq[PlanDefinition] = blocks.collect { case x: PlanDefinition => x }

    private lazy val groupLikes: Seq[GroupLike] = blocks.collect { case x: GroupLike => x }
    lazy val versioners: Seq[VersionerDef] = groupLikes.collect { case x: VersionerDef => x } //groupLikes.filter(_.keyword == "versioner")
    lazy val submitters: Seq[SubmitterDef] = groupLikes.collect { case x: SubmitterDef => x } //groupLikes.filter(_.keyword == "submitter")
    lazy val summaries: Seq[SummaryDef] = groupLikes.collect { case x: SummaryDef => x } //groupLikes.filter(_.keyword == "summary")

    private lazy val configLikes: Seq[ConfigDefinition] = blocks.collect { case x: ConfigDefinition => x }
    lazy val configs: Seq[ConfigDefinition] = configLikes.filter { t: ConfigDefinition => t.keyword == "config"}
    lazy val globalBlocks: Seq[ConfigDefinition] = configLikes.filter { t: ConfigDefinition => t.keyword == "global"}
    lazy val globals: Seq[ConfigAssignment] = globalBlocks.flatMap { _.lines }

    private lazy val taskLikes: Seq[TaskLike] = blocks.collect { case x: TaskLike => x }
    lazy val tasks: Seq[TaskDef] = taskLikes.collect{ case x: TaskDef => x } //taskDefs.filter { t: TaskDef => t.keyword == "task" }
    lazy val packages: Seq[PackageDef] = taskLikes.collect{ case x: PackageDef => x } //taskDefs.filter { t: TaskDef => t.keyword == "package" }

    /**
     * Explicitly incorporate task definitions which were
     * created by means of function calls.
     */
    private[syntax] def collapseFunctionCallTasks() : WorkflowDefinition = {

      // Gather the list of function calls
      val calls: Seq[CallDefinition] = blocks.collect { case x: CallDefinition => x }

      // Map from function names to function definitions
      val funcs: Map[Namespace,FuncDef] = {
        // Gather a list of those TaskDef objects that represent functions
        taskLikes.collect{ case x:FuncDef => x }. //taskLikes.filter { t: TaskLike => t.keyword == "func" }.
        // then make each element in the list a tuple,
        // where the first element is the function name
        // and the second element is the TaskDef object (that is, an AST node)
        map { t: FuncDef => (t.name, t) }.
        // then convert this list of tuples into a map
        // where each TaskDef can be retrieved via its name
        toMap
      }

      // Construct a list of new concrete task definitions
      val funcTasks = calls.map { functionCall: CallDefinition =>
        // Use the function name to look up where that function is defined
        val functionDefinition = funcs(functionCall.functionName)
        // TODO: XXX: Lane: This is probably broken for namespaces
        val taskName = Namespace.fromString(functionCall.name)
        // then create a new concrete task definition
        // using the bash code from the function definition
        // and the concrete inputs and parameters from the function call
        new TaskDef(taskName, functionDefinition, functionCall)
      }

      return new WorkflowDefinition(this.elements ++ funcTasks, this.files, this.hadImports, this.isImported)
    }

    def anonymousConfig: Option[ConfigDefinition] = configs.find(_.name == None)

    // imports will always be collapsed for the outside world
    private lazy val imported: Seq[WorkflowDefinition] = elements.collect { case x: WorkflowDefinition => x }
    private lazy val hasImports: Boolean = imported.size > 0
    lazy val usesImports: Boolean = isImported || hasImports || hadImports
    private[syntax] def collapseImports() = new WorkflowDefinition(
      blocks ++ imported.flatMap(_.blocks),
      files ++ imported.flatMap(_.files),
      hadImports=this.usesImports,
      isImported=this.isImported
    )

    override def toString() = blocks.mkString("\n\n")

    def ++(other: WorkflowDefinition) = new WorkflowDefinition(blocks ++ other.blocks, files ++ other.files, hadImports, isImported)
  }
}
