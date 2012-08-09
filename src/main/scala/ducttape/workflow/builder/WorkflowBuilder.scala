package ducttape.workflow.builder

import WorkflowBuilder.InputMode
import WorkflowBuilder.ParamMode
import WorkflowBuilder.ResolveMode
import ducttape.hyperdag.meta.PhantomMetaHyperDagBuilder
import ducttape.hyperdag.PackedVertex
import ducttape.syntax.AbstractSyntaxTree.ASTType
import ducttape.syntax.AbstractSyntaxTree.BranchGraft
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.BranchPointRef
import ducttape.syntax.AbstractSyntaxTree.Comments
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
import ducttape.syntax.AbstractSyntaxTree.CrossProduct
import ducttape.syntax.AbstractSyntaxTree.Literal
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.PlanDefinition
import ducttape.syntax.AbstractSyntaxTree.Sequence
import ducttape.syntax.AbstractSyntaxTree.SequentialBranchPoint
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.SubmitterDef
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.TaskHeader
import ducttape.syntax.AbstractSyntaxTree.TaskVariable
import ducttape.syntax.AbstractSyntaxTree.Unbound
import ducttape.syntax.AbstractSyntaxTree.VersionerDef
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.syntax.AbstractSyntaxTreeException
import ducttape.syntax.BashCode
import ducttape.syntax.FileFormatException
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPoint
import ducttape.workflow.BranchPointFactory
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.NoSuchBranchException
import ducttape.workflow.NoSuchBranchPointException
import ducttape.workflow.RealizationPlan
import ducttape.workflow.SpecGroup
import ducttape.workflow.Task
import ducttape.workflow.TaskTemplate
import ducttape.workflow.Types.PackedWorkVert
import ducttape.workflow.SpecTypes.SpecPair
import scala.collection.Seq
import scala.collection.Set
import scala.collection.Map
import scala.collection.mutable
import grizzled.slf4j.Logging

object WorkflowBuilder {

  class ResolveMode();
  case class InputMode() extends ResolveMode;
  case class ParamMode() extends ResolveMode;
  case class OutputMode() extends ResolveMode;

  /** alternates with BranchInfoTree */
  private[builder] class BranchPointTree(val branchPoint: BranchPoint) {
    val children = new mutable.ArrayBuffer[BranchTree]

    def getOrAdd(br: Branch): BranchTree = children.find { child => child.branch == br } match {
      case Some(found) => found
      case None => {
        val child = new BranchTree(br)
        children += child
        child
      }
    }

    // recursively enumerate all specs in this tree
    def specs: Iterable[SpecPair] = children.flatMap { child =>
      child.terminalData.flatMap { data: TerminalData => data.specs } ++
        child.children.flatMap { grandchild: BranchPointTreeData => grandchild.tree.specs }
    }

    override def toString() = "(BP=" + branchPoint + ": " + children.mkString + ")"
  }

  private[builder] class BranchPointTreeData(
    val tree: BranchPointTree,
    val grafts: Seq[Branch]) {
    override def toString() = tree + "+grafts=[" + grafts.mkString(",") + "]"
  }

  /**
   * alternates with BranchPointTree
   * branch will be baseline for the root vertex
   */
  private[builder] class BranchTree(val branch: Branch) {

    // if populated at the root, indicates no branch points
    //   specs, organized by which task they originate from
    //   and then by what grafts apply for that parent
    // NOTE: These are the final resolved specs, appropriate for use within some realization
    //   they are used in the BranchTreeMap; the plain edges in the HyperDAG
    //   are populated by the *original* unresolved specs
    // In the end, each element of terminalData will become an edge
    //   within some hyperedge
    var terminalData = new mutable.ArrayBuffer[TerminalData]
    val children = new mutable.ArrayBuffer[BranchPointTreeData]

    def getOrAdd(bp: BranchPoint, grafts: Seq[Branch]): BranchPointTreeData = {
      children.find { child => child.tree.branchPoint == bp && child.grafts == grafts } match {
        case Some(found) => found
        case None => {
          val bpt = new BranchPointTree(bp)
          val child = new BranchPointTreeData(bpt, grafts)
          children += child
          child
        }
      }
    }

    def getOrAdd(task: Option[TaskDef], grafts: Seq[Branch], isParam: Boolean): TerminalData = {
      // TODO: Do we need to sort grafts so that we don't get multiple entries
      // if they're in different orders?
      terminalData.find { data => data.task == task && data.grafts == grafts && data.isParam == isParam } match {
        case Some(data) => data
        case None => {
          val result = new TerminalData(task, grafts, isParam)
          terminalData += result
          result
        }
      }
    }

    override def toString() = "(B=" + branch + " :: terminalData=" + terminalData.mkString(":") + " :: " + children + ")"
  }

  private[builder] class TerminalData(
    val task: Option[TaskDef],
    val grafts: Seq[Branch],
    val isParam: Boolean) {

    val specs = new mutable.ArrayBuffer[SpecPair]

    override def toString() = "(%s %s isParam=%s %s)".format(task, grafts, isParam, specs)
  }
}

/**
 * This is where the real magic happens of turning an Abstract Syntax Tree
 * into an immutable HyperWorkflow that everything else can use to perform actions.
 *
 * This builder constructs a MetaHyperDAG whose edges represent *temporal* relationships
 * between tasks (e.g. files). The builder resolves parameters as literals ahead of time and
 * associates them with "phantom" vertices that are hidden since they are trivially complete.
 */
class WorkflowBuilder(wd: WorkflowDefinition, configSpecs: Seq[ConfigAssignment], builtins: Seq[WorkflowDefinition])
  extends Logging {

  import WorkflowBuilder._

  val branchPointFactory = new BranchPointFactory
  val branchFactory = new BranchFactory(branchPointFactory)
  val dag = new PhantomMetaHyperDagBuilder[TaskTemplate, BranchPoint, Branch, SpecGroup]()

  def catcher[U](func: => U)(implicit ref: BranchPointRef) = try { func } catch {
    case e: NoSuchBranchPointException => {
      throw new FileFormatException("No such branch point: %s".format(e.msg), ref)
    }
    case e: NoSuchBranchException => {
      throw new FileFormatException("No such branch: %s".format(e.msg), ref)
    }
  }

  def buildPlans(planDefs: Seq[PlanDefinition]): Seq[RealizationPlan] = {
    planDefs.flatMap { planDef: PlanDefinition =>
      planDef.crossProducts.map { cross: CrossProduct =>
        val realizations: Map[BranchPoint, Set[String]] = cross.value.map { implicit ref: BranchPointRef =>
          catcher {
            val branchPoint: BranchPoint = branchPointFactory(ref.name)
            // TODO: Change branches back to Branch after we get the baseline/branch name duality hammered out?
            val branches: Set[String] = ref.branchNames.flatMap { element: ASTType =>
              element match {
                case l: Literal => Seq(l.value.toString)
                case s: Sequence => {
                  for (x: BigDecimal <- s.start to s.end by s.increment) yield {
                    x.toString
                  }
                }
                case e: ASTType => throw new AbstractSyntaxTreeException(e, "Element cannot be used to refer to a branch name")
              }
            }.toSet
            (branchPoint, branches) // map entry
          }
        }.toMap
        new RealizationPlan(planDef, cross.goals, realizations)
      }
    }
  }
  
  def getHyperedges(task: TaskTemplate, 
                    specPhantomV: PackedVertex[Option[TaskTemplate]],
                    curNode: BranchPointTreeData,
                    debugNesting: Seq[Branch])
                   (implicit toVertex: TaskDef => PackedVertex[Option[TaskTemplate]])
    : Seq[(Branch, Seq[(PackedVertex[Option[TaskTemplate]], SpecGroup)])] = {

    val possibleHyperedges = curNode.tree.children.map { branchChild: BranchTree =>

      val nestedBranchEdges: Seq[(PackedVertex[Option[TaskTemplate]], SpecGroup)] = {
        // we might have multiple branch point trees if there are different graft sets
        branchChild.children.map { bpChild: BranchPointTreeData =>
          // we have more than one branch point in a row: create a phantom V for each graft
          val branchPhantomV: PackedVertex[Option[TaskTemplate]]
            = dag.addPhantomVertex(comment = "Phantom:%s.%s.nestedBranch[%s]".format(task.name, branchChild.branch.toString, bpChild.grafts.mkString(",")))
          traverse(task, specPhantomV, bpChild, debugNesting ++ Seq(branchChild.branch), branchPhantomV)
          (branchPhantomV, SpecGroup.empty(grafts=bpChild.grafts))
        }
      }

      // branches with no further branch points nested under them
      // get normal edges attached to them, which lead back to previous
      // tasks
      // we distinguish between literals (as specified by .isParam)
      // and specs that depend on previous tasks (not params)
      val terminalEdges: Seq[(PackedVertex[Option[TaskTemplate]], SpecGroup)] = {
        branchChild.terminalData.map { data: TerminalData =>
          data.isParam match {
            case false => {
              data.task match {
                // has a temporal dependency on a previous task
                case Some(task) => (toVertex(task), new SpecGroup(data.specs, data.grafts))
                // depends on a non-task such as a config/global assignment
                case None => (specPhantomV, new SpecGroup(data.specs, data.grafts))
              }
            }
            // no temporal dependency
            case true => (specPhantomV, new SpecGroup(data.specs, data.grafts))
          }
        }
      }
      debug("Task=%s; Found nested edges: %s and terminal edges: %s".format(task, nestedBranchEdges, terminalEdges))
      (branchChild.branch, nestedBranchEdges ++ terminalEdges)
    }
    
    // result type: (PackedVertex[Option[TaskTemplate]], SpecGroup)
    possibleHyperedges.filter {
      // don't include hyperedges with zero source vertices
      case (branchInfo, edges) => edges.size > 0
    }
  }

  // Overview of some of the compicated things traverse must deal with here -- in traverse() and getHyperedges():
  // * Nested branch points
  // * Grafting inside branch point defs
  // * Grafting of the same branch point that is currently being defined, inside that branch point def
  // * Having the same branch with different graft sets (this happens frequently for Baseline.baseline)
  //   - choosing one branch must jointly activate all graft sets -- don't accidentally iterate over (branch, graftSet) in epsilon vertex
  // * Having different branches with the same graft set (consider 2 branches, 1 graft set)
  //   - if we accidentally have an epsilon vertex for each (branch, graftSet) pair, then the branches will collide and cancel out
  // * Having grafts at different levels of branch point nesting

  // add one metaedge per branch point
  // the Baseline branch point and baseline branch are automatically added by findTasks() in the first pass
  def traverse(task: TaskTemplate,
    specPhantomV: PackedVertex[Option[TaskTemplate]],
    curNode: BranchPointTreeData,
    nestedBranches: Seq[Branch],
    sinkV: PackedVertex[Option[TaskTemplate]])(implicit toVertex: TaskDef => PackedVertex[Option[TaskTemplate]]) {

    // the branch point associated with the meta edge being created
    val branchPoint = curNode.tree.branchPoint
    debug("Task=%s %s: BranchPointTreeData is %s".format(task, nestedBranches, curNode))

    // we create a phantom vertex when:
    // 1) we need an imaginary home for config specs
    // 2) we have more than one branch point in a row (nested branch points)
    val hyperedges: Seq[(Branch, Seq[(PackedVertex[Option[TaskTemplate]], SpecGroup)])]
      = getHyperedges(task, specPhantomV, curNode, nestedBranches)

    if (!hyperedges.isEmpty) {
      val name = "Epsilon:%s:%s".format(branchPoint.toString, task.name)
      debug("Task=%s %s: Adding metaedge '%s' for branchPoint %s to HyperDAG: Component hyperedges are: %s".
        format(task, name, nestedBranches, branchPoint, hyperedges))
      dag.addMetaEdge(branchPoint, hyperedges, sinkV, comment=name)
    } else {
      debug("Task=%s %s: No metaedge for branchPoint %s is needed (zero component hyperedges)".
        format(task, nestedBranches, branchPoint))
    }
  }

  // create dependency pointers based on workflow definition
  def build(): HyperWorkflow = {
    val confSpecs: Map[String, Spec] = configSpecs.map { a: ConfigAssignment => (a.spec.name, a.spec) }.toMap

    // first identify all branch points that are present in the workflow so that
    // we can identify and store which elements are branch points
    def findBranchPoints(element: ASTType) {
      element match {
        case BranchPointDef(nameOpt: Option[String], branchSpecs: Seq[Spec]) => {
          nameOpt match {
            case Some(branchPointName) => {
              val branchPoint = branchPointFactory.get(branchPointName)
              for ( (branchSpec, idx) <- branchSpecs.zipWithIndex) {
                val isBaseline = (idx == 0)
                val branch = branchFactory.get(branchSpec.name, branchPoint, isBaseline)
              }
            }
            case None => {
              throw new FileFormatException("Anonymous branch points are not yet supported", element)
            }
          }
        }
        case _ => ;
      }
      element.children.foreach(findBranchPoints(_))
    }
    findBranchPoints(wd)

    // resolver has no knowledge of DAGs nor the dag builder
    val resolver = new TaskTemplateBuilder(wd, confSpecs, branchPointFactory, branchFactory)

    // first, find temporal and structural dependencies among tasks and store them as an edge map
    // also, pre-resolve any non-temporal dependencies such as parameters
    val foundTasks: FoundTasks = resolver.findTasks()

    // == we've just completed our first pass over the workflow file and linked everything together ==

    val vertices = new mutable.HashMap[String, PackedVertex[Option[TaskTemplate]]]
    for (tt <- foundTasks.taskTemplates) {
      if (vertices.contains(tt.name)) {
        val prev: TaskTemplate = vertices(tt.name).value.get
        throw new FileFormatException("Duplicate task name: %s".format(tt.name),
          List(tt.taskDef, prev.taskDef))
      }
      vertices += tt.name -> dag.addVertex(tt, comment = tt.name)
    }
    implicit def toVertex(t: TaskDef): PackedVertex[Option[TaskTemplate]] = vertices(t.name)

    // now build a graph representation by adding converting to (meta/hyper) edges
    for (v: PackedVertex[Option[TaskTemplate]] <- vertices.values) {
      val taskT: TaskTemplate = v.value.get
      debug("Adding %s to HyperDAG".format(taskT))
      val nestedBranchInfo: BranchPointTreeData = foundTasks.parents(taskT)
      val specPhantomV: PackedVertex[Option[TaskTemplate]] = dag.addPhantomVertex(comment = "Phantom:%s.literals".format(taskT.name))
      traverse(taskT, specPhantomV, nestedBranchInfo, Nil, v)
    }

    // organize packages
    val packageDefs = wd.packages.map { p => (p.name, p) }.toMap
    val plans: Seq[RealizationPlan] = buildPlans(wd.plans)

    // TODO: More checking on submitters and versioners?
    val submitters: Seq[SubmitterDef] = wd.submitters ++ builtins.flatMap { b: WorkflowDefinition => b.submitters }
    val versioners: Seq[VersionerDef] = wd.versioners ++ builtins.flatMap { b: WorkflowDefinition => b.versioners }

    // TODO: For params, we can resolve these values *ahead*
    // of time, prior to scheduling (but keep relationship info around)
    // (i.e. parameter dependencies should not imply temporal dependencies)
    val x = new HyperWorkflow(dag.build(), packageDefs, plans, submitters, versioners, branchPointFactory, branchFactory)
    debug("Workflow has %d vertices".format(x.dag.size))
    x
  }
}
