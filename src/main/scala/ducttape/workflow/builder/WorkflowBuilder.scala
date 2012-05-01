package ducttape.workflow.builder

import WorkflowBuilder.CONFIG_TASK_DEF
import WorkflowBuilder.InputMode
import WorkflowBuilder.ParamMode
import WorkflowBuilder.ResolveMode
import ducttape.hyperdag.meta.MetaHyperDagBuilder
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
import ducttape.workflow.BranchInfo
import ducttape.workflow.BranchPoint
import ducttape.workflow.BranchPointFactory
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.NoSuchBranchException
import ducttape.workflow.NoSuchBranchPointException
import ducttape.workflow.RealizationPlan
import ducttape.workflow.Task
import ducttape.workflow.TaskTemplate
import scala.collection.Seq
import scala.collection.Set
import scala.collection.Map
import scala.collection.mutable
import grizzled.slf4j.Logging

object WorkflowBuilder {
   // TODO: Better error reporting here?
   val CONFIG_TASK_DEF = new TaskDef(
    comments=new Comments(None),
    keyword="builtin",
    name="CONFIGURATION_FILE", 
    header=new TaskHeader(Nil), 
    commands=new BashCode(""))

   class ResolveMode();
   case class InputMode() extends ResolveMode;
   case class ParamMode() extends ResolveMode;
   case class OutputMode() extends ResolveMode;
}

/**
 * This is where the real magic happens of turning an Abstract Syntax Tree
 * into an immutable HyperWorkflow that everything else can use to perform actions.
 */
class WorkflowBuilder(wd: WorkflowDefinition, configSpecs: Seq[ConfigAssignment], builtins: Seq[WorkflowDefinition])
  extends Logging {
  
  import WorkflowBuilder._
  
  val branchPointFactory = new BranchPointFactory
  val branchFactory = new BranchFactory(branchPointFactory)
  val dag = new MetaHyperDagBuilder[TaskTemplate,BranchPoint,BranchInfo,Seq[Spec]]
  
  def buildPlans(planDefs: Seq[PlanDefinition]): Seq[RealizationPlan] = {
    val result = new mutable.ArrayBuffer[RealizationPlan]
    
    for (planDef: PlanDefinition <- planDefs) {
      for (cross: CrossProduct <- planDef.crossProducts) {
        val realizations = new mutable.HashMap[BranchPoint, Set[Branch]]
        for (ref: BranchPointRef <- cross.value) {
          try {
            val branchPoint: BranchPoint = branchPointFactory(ref.name)
            val branches: Set[Branch] = ref.branchNames.flatMap(
                (element: ASTType) => element match {
                  case l: Literal => List(branchFactory(l.value, branchPoint))
                  case s: Sequence => {
                    for (x: BigDecimal <- (s.start).to(s.end).by(s.increment) ) yield {
                      branchFactory(x.toString, branchPoint)
                    }
                  }
                  case e: ASTType => throw new AbstractSyntaxTreeException(e,"Element cannot be used to refer to a branch name")
                }
            ).toSet
            realizations += branchPoint -> branches
          } catch {
            case e: NoSuchBranchPointException => {
              throw new FileFormatException("ERROR: No such branch point: %s".format(e.msg), ref)
            }
            case e: NoSuchBranchException => {
              throw new FileFormatException("ERROR: No such branch: %s".format(e.msg), ref)
            }
          }
        }
        result += new RealizationPlan(planDef.name, cross.goals, realizations)
      }
    }
    result
  }
  
  def findEdges(foundTasks: FoundTasks,
                task: TaskTemplate,
                branchInfo: BranchInfo,
                parentTaskDefs: Set[Option[TaskDef]]): Seq[(Option[PackedVertex[TaskTemplate]],Seq[Spec])] = {
    
    // create an edge within each hyperedge for each input associated with a task
    // so that we will know which realization to use at the source of each edge.
    // parameters may also be subject to branching, but will never point at a task directly
    // since parameters do not imply a temporal ordering among task vertices.
    val edges = new mutable.ArrayBuffer[(Option[PackedVertex[TaskTemplate]],Seq[Spec])]
  
    // find which inputs are attached to this branch point
    // unlike params (which are just lumped into an edge to a single phantom vertex),
    // we must match the parent task so that we attach to the correct hyperedge
    // this will be the payload associated with each plain edge in the MetaHyperDAG
    def findInputSpecs(parentTaskDef: TaskDef): Seq[Spec] = task.inputVals.filter {
      case (ipSpec, specBranchez) => { val specBranches: Map[Branch,(Spec,TaskDef)] = specBranchez
        specBranches.get(branchInfo.branch) match {
          case None => false
          case Some( (_, specParent: TaskDef) ) => {
            debug("Comparing: %s %s %s".format(specParent, parentTaskDef, specParent == parentTaskDef))
            specParent == parentTaskDef
          }
        }
      }
    }.map { case (ipSpec, specBranches) => ipSpec }
    
    // see notes for findInputSpecs()
    // we need only filter by branch since all params get lumped into a single phantom vertex
    // through a single hyperedge *per branch*
    def findParamSpecs(): Seq[Spec] = task.paramVals.filter {
      case (ipSpec, specBranchez) => { val specBranches: Map[Branch,(Spec,TaskDef)] = specBranchez
        specBranches.contains(branchInfo.branch)
      }
    }.map { case (ipSpec, specBranches) => ipSpec }
  
    // parents are stored as Options so that we can use None to indicate phantom parent vertices
    parentTaskDefs.foreach {
      case Some(CONFIG_TASK_DEF) => {
        // this includes only paths (not params) defined in a config
        //
        // config entries may in turn contain branches, which require
        // edges to reconstruct which inputs/parameters each realized task should use
        // the parent task *of the ConfigVariable* will be listed as the current task
        val ipSpecs = findInputSpecs(task.taskDef)
        debug("CURRENT BRANCH %s HAS IP SPECS FOR CONFIG: %s".format(branchInfo, ipSpecs.toList))
        debug("INPUT VALS: " + task.inputVals)
        debug("Found config task def only")
        edges.append( (None, ipSpecs) )
      }
      case Some(parentTaskDef) => {
      val parentVert = foundTasks.vertices(parentTaskDef.name)
      val ipSpecs = findInputSpecs(parentTaskDef)
      // add an edge for each parameter/input at task that originates from parentVert
      debug("IP specs for branch point %s are: %s".format(branchInfo, ipSpecs))
      edges.append( (Some(parentVert), ipSpecs) )
     }
     case None => {
       // We have a branch point (possibly baseline) using one of the following:
       // 1) literal paths
       // 2) any kind of param values (since params never induce temporal dependencies)
       //
       // however, params may still need to be statically resolved later, which is why
       // we use findInputParamSpecs(). unlike literal paths/params, 
       debug("Found literal paths or some type of params only")
       val ipSpecs = findParamSpecs() ++ findInputSpecs(task.taskDef) // we are our own parent
         edges.append( (None, ipSpecs) )
     }
    }
    edges
  }
  
  // create dependency pointers based on workflow definition
  // TODO: This method has become morbidly obese -- break it out into several methods
  def build(): HyperWorkflow = {

    val confSpecs: Map[String, Spec] = configSpecs.map{ass => (ass.spec.name, ass.spec)}.toMap
    val resolver = new WorkflowResolver(wd, confSpecs, dag, branchPointFactory, branchFactory)
     
    // TODO: How does the structure for param branch points get handled?!?!

    // first, find *temporal* dependencies among tasks and store them as an edge map
    // also, pre-resolve any non-temporal dependencies such as parameters
    // e.g. parameters that introduce branch points will not become edges in the MetaHyperDAG
    // in general, the MetaHyperDAG
    val foundTasks: FoundTasks = resolver.findTasks()

    // == we've just completed our first pass over the workflow file and linked everything together ==

    // now build a graph representation by adding converting to (meta/hyper) edges
    for (v <- foundTasks.vertices.values) {
      val task: TaskTemplate = v.value
       
      debug("Adding %s to HyperDAG".format(task))

      // add one metaedge per branch point
      // the Baseline branch point and baseline branch are automatically added by findTasks() in the first pass
      for (branchPoint <- foundTasks.branchPointsByTask(task.taskDef)) {
 
        // create a hyperedge list in the format expected by the HyperDAG API
        val hyperedges = new mutable.ArrayBuffer[(BranchInfo, Seq[(Option[PackedVertex[TaskTemplate]],Seq[Spec])])]
        for ( (branchInfo, parentTaskDefs) <- foundTasks.parents(task); if branchPoint == branchInfo.branch.branchPoint) {
          val edges = findEdges(foundTasks, task, branchInfo, parentTaskDefs)
          hyperedges.append( (branchInfo, edges) )
        }
        if (!hyperedges.isEmpty) {
          // NOTE: The meta edges are not necessarily phantom, but just have that option
          debug("Adding metaedge for branchPoint %s task %s to HyperDAG: Component hyperedges are: %s".format(branchPoint, task, hyperedges))
          dag.addPhantomMetaEdge(branchPoint, hyperedges, v)
        } else {
          debug("No metaedge for branchPoint %s at task %s is needed (zero component hyperedges)".format(branchPoint, task))
        }
      }
    }
     
    // organize packages
    val packageDefs = wd.packages.map{p => (p.name, p)}.toMap
    val plans: Seq[RealizationPlan] = buildPlans(wd.plans)
    
    // TODO: More checking on submitters and versioners?
    val submitters: Seq[SubmitterDef] = wd.submitters ++ builtins.flatMap{ b: WorkflowDefinition => b.submitters }
    val versioners: Seq[VersionerDef] = wd.versioners ++ builtins.flatMap{ b: WorkflowDefinition => b.versioners }

    // TODO: For params, we can resolve these values *ahead*
    // of time, prior to scheduling (but keep relationship info around)
    // (i.e. parameter dependencies should not imply temporal dependencies)
    new HyperWorkflow(dag.build(), packageDefs, plans, submitters, versioners, branchPointFactory, branchFactory)
  }
}
