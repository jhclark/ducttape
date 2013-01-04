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
import ducttape.workflow.BranchPoint
import ducttape.workflow.BranchPointFactory
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.NoSuchBranchException
import ducttape.workflow.NoSuchBranchPointException
import ducttape.workflow.RealizationPlan
import ducttape.workflow.Task
import ducttape.workflow.TaskTemplate
import ducttape.workflow.builder.WorkflowBuilder.BranchPointTree
import ducttape.workflow.builder.WorkflowBuilder.BranchPointTreeData
import ducttape.workflow.builder.WorkflowBuilder.BranchTree
import ducttape.workflow.builder.WorkflowBuilder.TerminalData
import ducttape.workflow.SpecTypes.SpecPair
import ducttape.workflow.SpecTypes.LiteralSpecPair

import collection.Seq
import collection.Set
import collection.Map
import collection.mutable

import grizzled.slf4j.Logging

// unlike WorkflowBuilder, we have no interaction with hyperdag framework here
private[builder] class TaskTemplateBuilder(
    wd: WorkflowDefinition,
    confSpecs: Map[String,Spec],
    branchPointFactory: BranchPointFactory,
    branchFactory: BranchFactory) extends Logging {
  
  def findTasks(): FoundTasks = {
    
    val tasks: Seq[TaskDef] = 
      // Concatenate regular task definitions
      // with task definitions constructed via function calls
      (wd.tasks ++ wd.functionCallTasks)

    val taskMap: Map[String,TaskDef] = {
      tasks.
      // Make each element in the list a tuple, 
      // where the first element is the task name
      // and the second element is the TaskDef object (that is, an AST node)
      map { t: TaskDef => (t.name, t) }.
      // then convert this list of tuples into a map
      // where each TaskDef can be retrieved via its name      
      toMap
    }
              
    val branchPoints = new mutable.ArrayBuffer[BranchPoint]
    val parents: Map[TaskTemplate,BranchPointTreeData] = tasks.map { taskDef: TaskDef =>
      val tree = new BranchPointTree(Task.NO_BRANCH_POINT)
      val treeData = new BranchPointTreeData(tree, Nil)
      val baselineTree = new BranchTree(Task.NO_BRANCH)
      tree.children += baselineTree

      // parameters are different than file dependencies in that they do not
      // add any temporal dependencies between tasks and therefore do not
      // add any edges in the MetaHyperDAG
      //
      // params have no effect on temporal ordering, but can affect derivation of branches
      // therefore, params are *always* rooted at phantom vertices, no matter what -- hence, we store None
      //
      // most of the heavy lifting in this method is done by resolveBranchPoint(), which determines which
      // source specs should be used for each of the input/parameter specs in this task. these are then
      // added at the correct position in the BranchPointTree
      for (paramSpec: Spec <- taskDef.params) {
        resolveBranchPoint(taskDef, paramSpec, taskMap, isParam=true)(
          baselineTree, Seq(Task.NO_BRANCH), Some(taskDef), paramSpec, Nil)(resolveVarFunc=resolveParam)
      }

      for (inSpec: Spec <- taskDef.inputs) {
        resolveBranchPoint(taskDef, inSpec, taskMap, isParam=false)(
          baselineTree, Seq(Task.NO_BRANCH), Some(taskDef), inSpec, Nil)(resolveVarFunc=resolveInput)
      }
            
      val paramVals: Seq[LiteralSpecPair] = tree.specs.filter(_.isParam).map { spec =>
        val literalSrcSpec = spec.srcSpec.asInstanceOf[LiteralSpec] // guaranteed to succeed since isParam
        new LiteralSpecPair(spec.origSpec, spec.srcTask, literalSrcSpec, isParam=true)
      }.toSeq

      val inputVals: Seq[SpecPair] = tree.specs.filter(_.isInput).map { spec =>
        new SpecPair(spec.origSpec, spec.srcTask, spec.srcSpec, isParam=false)
      }.toSeq

      val taskT = new TaskTemplate(taskDef, inputVals, paramVals)
      (taskT, treeData) // key, value for parents map
    }.toMap
    
    val taskTemplates: Seq[TaskTemplate] = parents.keys.toSeq
    new FoundTasks(taskTemplates, parents, branchPoints)
  }
  
  
  // define branch resolution as a function that takes some callback functions
  // since we use it for both parameter and input file resolution,
  // but it behaves slightly different for each (parameters don't
  // imply a temporal ordering between vertices)
  //
  // resolveBranchPoint first recursively traverses any branch points,
  // adding nodes to the branch point tree (prevTree) as neccessary,
  // such that the final recursive call to resolveBranchPoint()
  // is correctly positioned in the branch point tree such that we
  // can add specs to this, the appropriate node, in the tree
  //
  // to actually resolve the output spec or literal param spec that will be linked
  // with our task's input/param spec, we then call a helper function (resolveVarFunc)
  // to handle the specific sort of variable (input or param). that helper function will
  // return the resolved output spec or literal param spec and we will add it to the branch
  // point tree
  private def resolveBranchPoint[SpecT <: Spec]
    (taskDef: TaskDef, origSpec: Spec, taskMap: Map[String,TaskDef], isParam: Boolean)
    (prevTree: BranchTree, branchHistory: Seq[Branch], curTask: Option[TaskDef],
     curSpec: Spec, prevGrafts: Seq[Branch])
    (resolveVarFunc: (TaskDef, Map[String,TaskDef], Spec, Option[TaskDef]) => (SpecT, Option[TaskDef], Seq[Branch]) ) {

    debug("Task=%s: Recursively resolving potential branch point: %s @ %s".format(taskDef, curSpec, curTask))

    // TODO: Un-nest these methods

    // note: you can skip these method definitions to the match-case below
    // to first see their usage

    // create an internal node in the branch tree
    def handleBranchPoint(branchPointName: String,
                          branchSpecs: Seq[Spec],
                          isFromConfig: Boolean = false,
                          isFromSeq: Boolean = false) {
      
      val branchPoint = branchPointFactory.get(branchPointName)
      
      for ( (branchSpec, idx) <- branchSpecs.zipWithIndex) {

        debug("Task=%s; bp=%s; new branchSpec=%s".format(taskDef, branchPoint, branchSpec))
        val isBaseline = (idx == 0)
        val branch = branchFactory.get(branchSpec.name, branchPoint, isBaseline)

        // Statically enforce grafts that happen within a nested
        // branch point (occur in our branch tree)
        // This is especially important for grafted params, since these
        // grafts are enforced exclusively at compile-time (now)
        // and it just helps keep the generated graphs a bit cleaner
        val graftsOk = prevGrafts.find { graft: Branch => graft.branchPoint == branch.branchPoint } match {
          case Some(graft) => {
            debug("Task=%s; Enforcing static graft %s == %s".format(taskDef, graft, branch))
            graft == branch
          }
          case None => true
        }
        
        if (graftsOk) {
          val newHistory = branchHistory ++ Seq(branch)
          // check if this branch assignment spec's rvalue defines another branch point
          // Note that it might not even be an assignment, since we allow anonymous branch specs
          branchSpec.rval match {
            // we found a nested branch point -- recursively call resolveBranchPoint()
            case BranchPointDef(_,_) => {
              // no grafts needed/possible here
              val bpTreeData: BranchPointTreeData = prevTree.getOrAdd(branchPoint, prevGrafts)
              val branchTree: BranchTree = bpTreeData.tree.getOrAdd(branch)

              debug("Task=%s; branchPoint=%s; Spec %s contains a nested branch point. Branch tree is %s for branch spec.".format(taskDef, branchPoint, branchSpec, branchTree.branch))
              resolveBranchPoint(taskDef, origSpec, taskMap, isParam)(
                branchTree, newHistory, curTask, branchSpec, prevGrafts)(resolveVarFunc)
            }
            // no nested branch point, go ahead and resolve which spec we link to
            case _ => {
              // if it's not a branch point def, we must immediately get its grafts
              // to get them in the right place in the tree
              val (srcSpec, srcTaskDefOpt, myGrafts) = resolveVarFunc(taskDef, taskMap, branchSpec, curTask)
              // only use *prevGrafts* (the grafts needed to arrive at this BP)
              // instead of prevGrafts++myGrafts
              val bpTreeData: BranchPointTreeData = prevTree.getOrAdd(branchPoint, prevGrafts)
              val branchTree: BranchTree = bpTreeData.tree.getOrAdd(branch)

              debug("Task=%s; branchPoint=%s; Spec %s does not contain a nested branch point. Branch tree is %s for branch spec.".format(taskDef, branchPoint, branchSpec, branchTree.branch))
              handleNonBranchPointHelper(branchSpec, branchTree)(srcSpec, srcTaskDefOpt, myGrafts)
            }
          }
        }
      }
    }

    // create a leaf node in the branch tree
    def handleNonBranchPoint() {
      // the srcTaskDef is only specified if it implies a temporal dependency (i.e. not literals)
      val (srcSpec, srcTaskDefOpt, myGrafts) = resolveVarFunc(taskDef, taskMap, curSpec, curTask)
      handleNonBranchPointHelper(curSpec, prevTree)(srcSpec, srcTaskDefOpt, myGrafts)
    }

    // we've gotten to the right place in the branch point tree
    // and we've resolved a potential spec we should maybe link to
    // first, we check if it is a branch point or not -- if not, we just add it
    def handleNonBranchPointHelper(theSpec: Spec, myBranchTree: BranchTree)
        (srcSpec: Spec, srcTaskDefOpt: Option[TaskDef], myGrafts: Seq[Branch]) {

      // use myGrafts instead of myGrafts ++ prevGrafts
      val allGrafts: Seq[Branch] = myGrafts
      debug("Resolved %s to potentially non-branch spec %s @ %s with new grafts %s (all grafts: %s)".format(
        origSpec, srcSpec, srcTaskDefOpt, myGrafts, allGrafts))
      
      // resolveVarFunc might have returned a branch point to us
      // if it traced back to a parent's parameter, which is itself a branch point
      // if that's the case, we should continue recursing. otherwise, just add.
      srcSpec.rval match {
        // phew, we got either a literal (filename or parameter value)
        // or an unbound output (ducttape will determine the output file path of this task
        // that will be run in the future)
        case Literal(_) | Unbound() => {

          // TODO: We need to remove this branch from the history/branch tree entirely...
          // this potentially removes phantoms even
          // i.e. don't modify the grafts, modify the tree...
          // TODO: Figure out what the above comment means.

          // store specs at this branch nesting along with its grafts
          // this is used by the MetaHyperDAG to determine structure and temporal dependencies
          val data: TerminalData = myBranchTree.getOrAdd(srcTaskDefOpt, allGrafts, isParam)
          data.specs += new SpecPair(origSpec, srcTaskDefOpt, srcSpec, isParam)
          
          trace("Task=%s; Resolved %s --> %s (%s); Grafts are: %s".format(taskDef, origSpec, srcSpec, srcTaskDefOpt, allGrafts))
        }
        case _ => {
          // not a literal -- keep tracing through branch points
          // note that we now recurse with a different curTask
          resolveBranchPoint(taskDef, origSpec, taskMap, isParam)(
            myBranchTree, branchHistory, srcTaskDefOpt, srcSpec, allGrafts)(resolveVarFunc)
        }
      }
    }
    
    // generate the literal specs implied by a sequence branch point
    def generateBranchSpecs(bpName: String, start: BigDecimal, end: BigDecimal, inc: BigDecimal): Seq[LiteralSpec] = {
      for (value <- start to end by inc) yield {
        debug("Generating literal spec from sequential branch element: %s".format(value))
        new LiteralSpec(value.toString, new Literal(value.toString), dotVariable=false)
      }
    }
    
    def getName(branchPointNameOpt: Option[String], astElem: ASTType) = branchPointNameOpt match {
      case Some(name) => name
      case None => throw new FileFormatException("Branch point name is required", astElem)
    }

    curSpec.rval match {
      // we found another branch point -- keep recursing
      case bp @ BranchPointDef(branchPointNameOpt, branchSpecz) => {
        val branchSpecs = branchSpecz
        val branchPointName = getName(branchPointNameOpt, bp)
        handleBranchPoint(branchPointName, branchSpecs)
      }
      // we found another branch point -- keep recursing
      case bp @ SequentialBranchPoint(branchPointNameOpt: Option[_], sequence: Sequence) => {
        val branchPointName = getName(branchPointNameOpt, bp)
        val branchSpecs = generateBranchSpecs(branchPointName, sequence.start, sequence.end, sequence.increment)
        debug("Generated sequence branch specs: " + branchSpecs)
        handleBranchPoint(branchPointName, branchSpecs, isFromSeq=true)
      }
      // we found a global config variable -- we first have to look inside
      // that variable to determine what to do next
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
      // ha! we finally found a spec that isn't a branch point
      // we're done traversing through nested branch points
      // see if we can resolve the output spec or literal spec associated with our task's spec
      case _ => handleNonBranchPoint()
    }
  } // handleBranchPoint -- TODO: This method is obese, split it up.

  // the resolved Spec is guaranteed to be a literal for params
  private def resolveParam(taskDef: TaskDef, taskMap: Map[String,TaskDef], spec: Spec, curTask: Option[TaskDef]) = {
    resolveNonBranchVar(ParamMode())(taskDef, taskMap, spec)(src=curTask)
  }
   
  private def resolveInput(taskDef: TaskDef, taskMap: Map[String,TaskDef], spec: Spec, curTask: Option[TaskDef]) = {
    resolveNonBranchVar(InputMode())(taskDef, taskMap, spec)(src=curTask)
  }
  
  // group parameters via currying by (1) initial state and (2) items that change recursively
  // srcTaskDefDependency is only non-None if it implies a temporal dependency
  // returns (srcSpec, srcTaskDefDependency, grafts)
  //
  // TODO: Since things are curried in this way, we could assign the result of the function
  // with the first two groups of argument applied to a val -- then that function could just
  // call itself, modifying only the last 3 arguments
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
      // in handleNonBranchPointHelper()
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
      case Some(confSpec) => (confSpec, None, grafts)
      case None => throw new FileFormatException(
          "Config variable %s required by input %s at task %s not found in config file.".
            format(varName, spec.name, taskDef.name),
          List(spec))
    }
  }
}
