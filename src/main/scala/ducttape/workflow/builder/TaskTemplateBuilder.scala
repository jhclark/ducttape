package ducttape.workflow.builder

import WorkflowBuilder.InputMode
import WorkflowBuilder.ParamMode
import WorkflowBuilder.ResolveMode
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
import ducttape.workflow.builder.WorkflowBuilder.BranchPointTree
import ducttape.workflow.builder.WorkflowBuilder.BranchInfoTree
import ducttape.workflow.builder.WorkflowBuilder.TerminalData
import ducttape.workflow.SpecTypes.SpecPair
import ducttape.workflow.SpecTypes.LiteralSpecPair
import scala.collection.Seq
import scala.collection.Set
import scala.collection.Map
import scala.collection.mutable
import grizzled.slf4j.Logging

// unlike WorkflowBuilder, we have no interaction with hyperdag framework here
private[builder] class TaskTemplateBuilder(
    wd: WorkflowDefinition,
    confSpecs: Map[String,Spec],
    branchPointFactory: BranchPointFactory,
    branchFactory: BranchFactory) extends Logging {
  
  def findTasks(): FoundTasks = {
    
    val taskMap: Map[String,TaskDef] = wd.tasks.map { t: TaskDef => (t.name, t) } toMap
    
    val branchPoints = new mutable.ArrayBuffer[BranchPoint]
    val parents: Map[TaskTemplate,BranchPointTree] = wd.tasks.map { taskDef: TaskDef =>
      val tree = new BranchPointTree(Task.NO_BRANCH_POINT)
      val baselineTree = new BranchInfoTree(Task.NO_BRANCH)
      tree.children += baselineTree

      // parameters are different than file dependencies in that they do not
      // add any temporal dependencies between tasks and therefore do not
      // add any edges in the MetaHyperDAG
      //
      // TODO: XXX:
      // params have no effect on temporal ordering, but can affect derivation of branches
      // therefore, params are *always* rooted at phantom vertices, no matter what (hence, None)
      for (paramSpec: Spec <- taskDef.params) {
        resolveBranchPoint(taskDef, paramSpec, taskMap, isParam=true)(
          baselineTree, Seq(Task.NO_BRANCH), Some(taskDef), paramSpec, Nil)(resolveVarFunc=resolveParam)
      }

      for (inSpec: Spec <- taskDef.inputs) {
        resolveBranchPoint(taskDef, inSpec, taskMap, isParam=false)(
          baselineTree, Seq(Task.NO_BRANCH), Some(taskDef), inSpec, Nil)(resolveVarFunc=resolveInput)
      }
      
      val inputVals: Seq[SpecPair] = tree.specs.filter(_.isInput).map { spec =>
        new SpecPair(spec.origSpec, spec.srcTask, spec.srcSpec, isParam=false)
      }.toSeq
      
      val paramVals: Seq[LiteralSpecPair] = tree.specs.filter(_.isParam).map { spec =>
        val literalSrcSpec = spec.srcSpec.asInstanceOf[LiteralSpec] // guaranteed to succeed since isParam
        new LiteralSpecPair(spec.origSpec, spec.srcTask, literalSrcSpec, isParam=true)
      }.toSeq

      val taskT = new TaskTemplate(taskDef, inputVals, paramVals)
      (taskT, tree) // key, value for parents map
    }.toMap
    
    val taskTemplates: Seq[TaskTemplate] = parents.keys.toSeq
    new FoundTasks(taskTemplates, parents, branchPoints)
  }
  
  
  // define branch resolution as a function that takes some callback functions
  // since we use it for both parameter and input file resolution,
  // but it behaves slightly different for each (parameters don't
  // imply a temporal ordering between vertices)
  //
  // resolveBranchPoint first weeds out any branch points
  // then calls a helper function (resolveVarFunc) to handle the specific
  // sort of variable
  def resolveBranchPoint[SpecT <: Spec]
    (taskDef: TaskDef, origSpec: Spec, taskMap: Map[String,TaskDef], isParam: Boolean)
    (prevTree: BranchInfoTree, branchHistory: Seq[Branch], curTask: Option[TaskDef],
     curSpec: Spec, prevGrafts: Seq[Branch])
    (resolveVarFunc: (TaskDef, Map[String,TaskDef], Spec, Option[TaskDef]) => (SpecT, Option[TaskDef], Seq[Branch]) ) {

    debug("Task=%s: Recursively resolving potential branch point: %s @ %s".format(taskDef, curSpec, curTask))
    
    // create an internal node in the branch tree
    def handleBranchPoint(branchPointName: String,
                          branchSpecs: Seq[Spec],
                          isFromConfig: Boolean = false,
                          isFromSeq: Boolean = false) {
      
      val branchPoint = branchPointFactory.get(branchPointName)
      val bpTree: BranchPointTree = prevTree.getOrAdd(branchPoint)
      
      for (branchSpec: Spec <- branchSpecs) {
        debug("branchSpec = " + branchSpec)
        val branch = branchFactory.get(branchSpec.name, branchPoint)
        val branchTree = bpTree.getOrAdd(branch)
        val newHistory = branchHistory ++ Seq(branch)
        resolveBranchPoint(taskDef, origSpec, taskMap, isParam)(
          branchTree, newHistory, curTask, branchSpec, prevGrafts)(resolveVarFunc)
      }
    }

    // create a leaf node in the branch tree
    def handleNonBranchPoint() {
      // the srcTaskDef is only specified if it implies a temporal dependency (i.e. not literals)
      val (srcSpec, srcTaskDefOpt, myGrafts) = resolveVarFunc(taskDef, taskMap, curSpec, curTask)
      val allGrafts = myGrafts ++ prevGrafts
      debug("Resolved %s to potentially non-branch spec %s @ %s with grafts %s".format(
        origSpec, srcSpec, srcTaskDefOpt, myGrafts))
      
      // resolveVarFunc might have returned a branch point to us
      // if it traced back to a parent's parameter, which is itself a branch point
      // if that's the case, we should continue recursing. otherwise, just add.
      srcSpec.rval match {
        case Literal(_) | Unbound() => {
          // store specs at this branch nesting along with its grafts
          // this is used by the MetaHyperDAG to determine structure and temporal dependencies
          val data: TerminalData = prevTree.getOrAdd(srcTaskDefOpt, allGrafts)
          data.specs += new SpecPair(origSpec, srcTaskDefOpt, srcSpec, isParam)
          
          //debug("Setting grafts for %s: %s".format(origSpec, allGrafts))
        }
        case _ => {
          // not a literal -- keep tracing through branch points
          // note that we now recurse with a different curTask
          resolveBranchPoint(taskDef, origSpec, taskMap, isParam)(
            prevTree, branchHistory, srcTaskDefOpt, srcSpec, allGrafts)(resolveVarFunc)
        }
      }
    }
       
    def generateBranchSpecs(bpName: String, start: BigDecimal, end: BigDecimal, inc: BigDecimal): Seq[LiteralSpec] = {
      for (value <- start to end by inc) yield {
        new LiteralSpec(bpName.toLowerCase + value.toString, new Literal(value.toString), dotVariable=false)
      }
    }
    
    def getName(branchPointNameOpt: Option[String], astElem: ASTType) = branchPointNameOpt match {
      case Some(name) => name
      case None => throw new FileFormatException("Branch point name is required", astElem)
    }

    curSpec.rval match {
      case bp @ BranchPointDef(branchPointNameOpt, branchSpecz) => {
        val branchSpecs = branchSpecz
        val branchPointName = getName(branchPointNameOpt, bp)
        handleBranchPoint(branchPointName, branchSpecs)
      }
      case bp @ SequentialBranchPoint(branchPointNameOpt: Option[_], sequence: Sequence) => {
        val branchPointName = getName(branchPointNameOpt, bp)
        val branchSpecs = generateBranchSpecs(branchPointName, sequence.start, sequence.end, sequence.increment)
        handleBranchPoint(branchPointName, branchSpecs, isFromSeq=true)
      }
      case ConfigVariable(varName) => {
        confSpecs.get(varName) match {
          case Some(confSpec) => {
            resolveBranchPoint(taskDef, origSpec, taskMap, isParam)(
              prevTree, branchHistory, curTask, confSpec, prevGrafts)(resolveVarFunc)
          }
          case None => throw new FileFormatException(
            "Config variable %s required by input %s at task %s not found in config file.".
              format(varName, curSpec.name, taskDef.name),
            curSpec)
        }
      }
      case _ => handleNonBranchPoint()
    }
  }

  // the resolved Spec is guaranteed to be a literal for params
  private def resolveParam(taskDef: TaskDef, taskMap: Map[String,TaskDef], spec: Spec, curTask: Option[TaskDef]) = {
    resolveNonBranchVar(ParamMode())(taskDef, taskMap, spec)(src=curTask)
  }
   
  private def resolveInput(taskDef: TaskDef, taskMap: Map[String,TaskDef], spec: Spec, curTask: Option[TaskDef]) = {
    resolveNonBranchVar(InputMode())(taskDef, taskMap, spec)(src=curTask)
  }
  
  // TODO: document what's going on here -- maybe move elsewhere
  // group parameters by (1) initial state and (2) items that change recursively
  // srcTaskDefDependency is only non-None if it implies a temporal dependency
  // returns (srcSpec, srcTaskDefDependency, grafts)
  private def resolveNonBranchVar(mode: ResolveMode)
                                 (origTaskDef: TaskDef,
                                  taskMap: Map[String,TaskDef], spec: Spec)
                                 (curSpec: Spec=spec,
                                  src: Option[TaskDef],
                                  grafts: Seq[Branch] = Nil)
                                 : (Spec, Option[TaskDef], Seq[Branch]) = {
    curSpec.rval match {
      // we might have traced back through a TaskVariable into a parent's parameters,
      // which can, in turn, define a branch point
      // just return what we have and let resolveBranchPoint figure out the rest
      case BranchPointDef(_,_) | SequentialBranchPoint(_,_) => (curSpec, src, grafts)
      
      // literals will never have a use for grafts
      case Literal(litValue) => {
        val litSpec = curSpec.asInstanceOf[LiteralSpec]
        val litSrc = if (src != Some(origTaskDef) || spec != curSpec) src else None
        (litSpec, litSrc, Nil)
      }
      
      case ConfigVariable(varName) => resolveConfigVar(varName, origTaskDef, spec, src, grafts)
      
      case TaskVariable(srcTaskName, srcOutName) => resolveTaskVar(mode)(
        origTaskDef, taskMap, spec)(curSpec, src, grafts)(srcTaskName, srcOutName)
      
      case BranchGraft(srcOutName, srcTaskNameOpt, branchGraftElements) => {
        val (srcSpec, srcTask, prevGrafts) = srcTaskNameOpt match {
          case Some(srcTaskName) => {
            resolveTaskVar(mode)(origTaskDef, taskMap, spec)(curSpec, src, grafts)(srcTaskName, srcOutName)
          }
          case None => resolveConfigVar(srcOutName, origTaskDef, spec, src, grafts)
        }
        val resultGrafts = prevGrafts ++ branchGraftElements.map{ e => try {
            branchFactory(e.branchName, e.branchPointName)
          } catch {
            case ex: NoSuchBranchException => throw new FileFormatException(ex.getMessage, e)
          }
        }
        (srcSpec, srcTask, resultGrafts)
      }
      
      case Unbound() => {
        mode match {
          case InputMode() => {
            // make sure we didn't just refer to ourselves -- 
            // referring to an unbound output of a parent task is fine though (and usual)
            if (src != Some(origTaskDef) || spec != curSpec) {
              (curSpec, src, grafts)
            } else {
              debug("Original task was %s and src is %s".format(origTaskDef, src))
              throw new FileFormatException("Unbound input variable: %s".format(curSpec.name),
                                            List(origTaskDef, curSpec))
            }
          }
          case _ => throw new RuntimeException("Unsupported unbound variable: %s".format(curSpec.name))
        }
      }
      
    }
  }
   
  // helper for resolveNonBranchVar
  def resolveTaskVar(mode: ResolveMode)
                    (origTaskDef: TaskDef, taskMap: Map[String,TaskDef], spec: Spec)
                    (curSpec: Spec, prevTask: Option[TaskDef], grafts: Seq[Branch])
                    (srcTaskName: String, srcOutName: String)
                    : (Spec, Option[TaskDef], Seq[Branch]) = {
     
    taskMap.get(srcTaskName) match {
      case Some(srcDef: TaskDef) => {
        // determine where to search when resolving this variable's parent spec
        val specSet = mode match {
          case InputMode() => srcDef.outputs
          case ParamMode() => srcDef.params
        }
        // search for the parent spec
        specSet.find(outSpec => outSpec.name == srcOutName) match {
          case Some(srcSpec) => {
            debug("Found parent of %s @ %s => %s @ %s".format(spec, srcTaskName, srcSpec, srcDef))
            resolveNonBranchVar(mode)(origTaskDef, taskMap, spec)(srcSpec, Some(srcDef), grafts)
          }
          case None => {
            // give a very specific error if it was defined in the task but just not the correct output/param set
            srcDef.allSpecs.find(outSpec => outSpec.name == srcOutName) match {
              case Some(_) => throw new FileFormatException(
              "Output %s at source task %s for required by input %s at task %s not found. It was declared at the source task, but has the wrong type. Candidates are: %s".
                format(srcOutName, srcTaskName, spec.name, origTaskDef.name, specSet.map(_.name).mkString(" ")),
              List(spec, srcDef))
              case None => throw new FileFormatException(
              "Output %s at source task %s for required by input %s at task %s not found. Candidates are: %s".
                format(srcOutName, srcTaskName, spec.name, origTaskDef.name, specSet.map(_.name).mkString(" ")),
              List(spec, srcDef))
            }
          }
        }
      }
      case None => {
        throw new FileFormatException(
          "Source task %s for input %s at task %s not found".
            format(srcTaskName, spec.name, origTaskDef.name), spec)
      }
    }
  }
   
  def resolveConfigVar(varName: String, taskDef: TaskDef, spec: Spec, src: Option[TaskDef], grafts: Seq[Branch]) = {
    confSpecs.get(varName) match {
      // TODO: Should we return? Or do we allow config files to point back into workflows?
      case Some(confSpec) => (confSpec, None, grafts)
      case None => throw new FileFormatException(
          "Config variable %s required by input %s at task %s not found in config file.".
            format(varName, spec.name, taskDef.name),
          List(spec))
    }
  }
}