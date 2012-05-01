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

private[builder] class WorkflowResolver(
    wd: WorkflowDefinition,
    confSpecs: Map[String,Spec],
    dag: MetaHyperDagBuilder[TaskTemplate,BranchPoint,BranchInfo,Seq[Spec]],
    branchPointFactory: BranchPointFactory,
    branchFactory: BranchFactory) {
  
  def findTasks(): FoundTasks = {
    
    val taskMap = new mutable.HashMap[String,TaskDef]
    for (t: TaskDef <- wd.tasks) {
      taskMap += t.name -> t
    }

    val parents = new mutable.HashMap[TaskTemplate,Map[BranchInfo,mutable.Set[Option[TaskDef]]]]
    val vertices = new mutable.HashMap[String,PackedVertex[TaskTemplate]]
    val branchPoints = new mutable.ArrayBuffer[BranchPoint]
    val branchPointsByTask = new mutable.HashMap[TaskDef,mutable.Set[BranchPoint]]
    
    for (taskDef <- wd.tasks) {
       // TODO: Check for all inputs/outputs/param names being unique in this step
       if (vertices.contains(taskDef.name)) {
         val prev: TaskTemplate = vertices(taskDef.name).value
         throw new FileFormatException("Duplicate task name: %s".format(taskDef.name),
                                       List(taskDef, prev.taskDef))
       }

       val parentsByBranch = new mutable.HashMap[BranchInfo,mutable.Set[Option[TaskDef]]]

       // parameters are different than file dependencies in that they do not
       // add any temporal dependencies between tasks and therefore do not
       // add any edges in the MetaHyperDAG
       val paramVals = new mutable.ArrayBuffer[(Spec,Map[Branch,(LiteralSpec,TaskDef)])](taskDef.params.size)
       for (paramSpec: Spec <- taskDef.params) {
         resolveBranchPoint(taskDef, paramSpec, taskMap, branchPoints, branchPointsByTask, paramVals,
           // params have no effect on temporal ordering, but can affect derivation of branches
           // therefore, params are *always* rooted at phantom vertices, no matter what (hence, None)
           recordParentsFunc = (branchInfo: BranchInfo, taskDef: Option[TaskDef])
             => {parentsByBranch.getOrElseUpdate(branchInfo, {new mutable.HashSet}) += None},
           resolveVarFunc=resolveParam)
       }

       val inputVals = new mutable.ArrayBuffer[(Spec,Map[Branch,(Spec,TaskDef)])](taskDef.inputs.size)
       for (inSpec: Spec <- taskDef.inputs) {
         resolveBranchPoint(taskDef, inSpec, taskMap, branchPoints, branchPointsByTask, inputVals,
             // make a note of what edges we'll need to add later
             recordParentsFunc = (branchInfo: BranchInfo, srcTaskDef: Option[TaskDef])
               => {parentsByBranch.getOrElseUpdate(branchInfo, {new mutable.HashSet}) += srcTaskDef},
             resolveVarFunc=resolveInput)
       }

       if (paramVals.size == 0 && inputVals.size == 0) {
         // we MUST inherit from at least the baseline branch point
         branchPointsByTask.getOrElseUpdate(taskDef, {new mutable.HashSet}) += Task.NO_BRANCH_POINT
       }

       val task = new TaskTemplate(taskDef, branchPointsByTask(taskDef).toSeq, inputVals, paramVals)
       parents += task -> parentsByBranch
       vertices += task.name -> dag.addVertex(task)
     }
    new FoundTasks(parents, vertices, branchPoints, branchPointsByTask)
  }

   // the resolved Spec is guaranteed to be a literal for params
   private def resolveParam(taskDef: TaskDef, taskMap: Map[String,TaskDef], spec: Spec)
                           : (LiteralSpec, TaskDef, Seq[Branch]) = {
     resolveVar(ParamMode())(taskDef, taskMap, spec)().asInstanceOf[(LiteralSpec,TaskDef,Seq[Branch])]
   }
   
   private def resolveInput(taskDef: TaskDef, taskMap: Map[String,TaskDef], spec: Spec) = {
     resolveVar(InputMode())(taskDef, taskMap, spec)()
   }
   
   // helper for resolveVar
   def resolveTaskVar(mode: ResolveMode)
                     (taskDef: TaskDef, taskMap: Map[String,TaskDef], spec: Spec)
                     (curSpec: Spec, src: TaskDef, grafts: Seq[Branch])
                     (srcTaskName: String, srcOutName: String)
                     : (Spec, TaskDef, Seq[Branch]) = {
     
     taskMap.get(srcTaskName) match {
       case Some(srcDef: TaskDef) => {
         // determine where to search when resolving this variable's parent spec
         val specSet = mode match {
           case InputMode() => srcDef.outputs
           case ParamMode() => srcDef.params
         }
         // search for the parent spec
         specSet.find(outSpec => outSpec.name == srcOutName) match {
           case Some(srcSpec) => resolveVar(mode)(taskDef, taskMap, spec)(srcSpec, srcDef, grafts)
           case None => {
             throw new FileFormatException(
               "Output %s at source task %s for required by input %s at task %s not found. Candidate outputs are: %s".
                 format(srcOutName, srcTaskName, spec.name, taskDef.name, specSet.map(_.name).mkString(" ")),
               List(spec, srcDef))
           }
         }
       }
       case None => {
         throw new FileFormatException(
           "Source task %s for input %s at task %s not found".
           format(srcTaskName, spec.name, taskDef.name), spec)
       }
     }
   }
   
   def resolveConfigVar(varName: String, taskDef: TaskDef, spec: Spec, src: TaskDef, grafts: Seq[Branch]) = {
     confSpecs.get(varName) match {
       // TODO: Does this TaskDef break line numbering for error reporting?
       // TODO: Should we return? Or do we allow config files to point back into workflows?
       case Some(confSpec) => (confSpec, CONFIG_TASK_DEF, grafts)
       case None => throw new FileFormatException(
           "Config variable %s required by input %s at task %s not found in config file.".
             format(varName, spec.name, taskDef.name),
           List(spec, src))
     }
   }

   // TODO: document what's going on here -- maybe move elsewhere
   // group parameters by (1) initial state and (2) items that change recursively
   private def resolveVar(mode: ResolveMode)
                         (taskDef: TaskDef, taskMap: Map[String,TaskDef], spec: Spec)
                         (curSpec: Spec = spec, src: TaskDef = taskDef, grafts: Seq[Branch] = Nil)
                         : (Spec, TaskDef, Seq[Branch]) = {
     
     curSpec.rval match {
       case Literal(litValue) => {
         // TODO: can we enforce this as part of the match instead of using a cast?
         val litSpec = curSpec.asInstanceOf[LiteralSpec] // guaranteed to succeed
         (litSpec, src, Nil) // literals will never have a use for grafts
       }
       case ConfigVariable(varName) => resolveConfigVar(varName, taskDef, spec, src, grafts)
       case TaskVariable(srcTaskName, srcOutName) => {
         resolveTaskVar(mode)(taskDef, taskMap, spec)(curSpec, src, grafts)(srcTaskName, srcOutName)
       }
       case BranchGraft(srcOutName, srcTaskNameOpt, branchGraftElements) => {
         val (srcSpec, srcTask, prevGrafts) = srcTaskNameOpt match {
           case Some(srcTaskName) => {
             resolveTaskVar(mode)(taskDef, taskMap, spec)(curSpec, src, grafts)(srcTaskName, srcOutName)
           }
           case None => {
             resolveConfigVar(srcOutName, taskDef, spec, src, grafts)
           }
         }
         val resultGrafts = prevGrafts ++ branchGraftElements.map{ e => try {
             branchFactory(e.branchName, e.branchPointName)
           } catch {
             case ex: NoSuchBranchException => throw new FileFormatException(ex.getMessage, e)
           }
         }
         (srcSpec, srcTask, resultGrafts)
       }
       case BranchPointDef(_,_) => throw new RuntimeException("Expected branches to be resolved by now")
       case SequentialBranchPoint(_,_) => {
         throw new RuntimeException("Expected branches to be resolved by now")
       }
       case Unbound() => {
         mode match {
           case InputMode() => {
             // make sure we didn't just refer to ourselves -- 
             // referring to an unbound output is fine though (and usual)
             if (src != taskDef || spec != curSpec) {
               (curSpec, src, grafts)
             } else {
               throw new FileFormatException("Unbound input variable: %s".format(curSpec.name),
                                             List(taskDef, src))
             }
           }
           case _ => throw new RuntimeException("Unsupported unbound variable: %s".format(curSpec.name))
         }
       }
     }
   }
   
   
   // define branch resolution as a function that takes some callback functions
   // since we use it for both parameter and input file resolution,
   // but it behaves slightly different for each (parameters don't
   // imply a temporal ordering between vertices)
   def resolveBranchPoint[SpecT](taskDef: TaskDef,
                                 inSpec: Spec,
                                 defMap: Map[String,TaskDef],
                                 branchPoints: mutable.ArrayBuffer[BranchPoint],
                                 branchPointsByTask: mutable.HashMap[TaskDef,mutable.Set[BranchPoint]],
                                 resolvedVars: mutable.ArrayBuffer[(Spec,Map[Branch,(SpecT,TaskDef)])],
                                 recordParentsFunc: (BranchInfo,Option[TaskDef]) => Unit,
                                 resolveVarFunc: (TaskDef, Map[String,TaskDef], Spec) => (SpecT, TaskDef, Seq[Branch]) ) = {

     def handleBranchPoint(branchPointName: String,
                           branchSpecs: Seq[Spec],
                           // TODO: XXX: Nasty hack for config branch points:
                           // TODO: We should really be creating some sort of phantom vertices/tasks for config lines
                           confForcedSrc: Option[TaskDef] = None) = {
         // TODO: If a branch point is *REDECLARED* in a workflow
         // assert that it has exactly the same branches in all locations
         // else die (and that the baseline is the same -- i.e. in the same position)
         // This must be done for the paramSpecs above as well

         val branchPoint = branchPointFactory.get(branchPointName)
         val branchMap = new mutable.HashMap[Branch, (SpecT,TaskDef)]
         for (branchSpec <- branchSpecs) {
           val branch = branchFactory.get(branchSpec.name, branchPoint)
           val (srcSpec, srcTaskDef, grafts) = resolveVarFunc(taskDef, defMap, branchSpec)
           branchMap.put(branch, (srcSpec, srcTaskDef) )
           
           val branchInfo = new BranchInfo(branch, grafts)
           if (confForcedSrc != None) {
             // XXX: Yes, we link this to 2 branches
             recordParentsFunc(branchInfo, confForcedSrc)
           } else if (srcTaskDef != taskDef) { // don't create cycles
             recordParentsFunc(branchInfo, Some(srcTaskDef))
           } else {
             recordParentsFunc(branchInfo, None)
           }
         }

         // TODO: Rework this so that branch points can be associated with multiple tasks?
         // Right now the constraintFilter is taking care of this at traversal time
         branchPoints += branchPoint
         branchPointsByTask.getOrElseUpdate(taskDef, {new mutable.HashSet}) += branchPoint
         resolvedVars.append( (inSpec, branchMap) )
     }

     def handleNonBranchPoint() {
       val (srcSpec, srcTaskDef, grafts) = resolveVarFunc(taskDef, defMap, inSpec)
       resolvedVars.append( (inSpec, Map(Task.NO_BRANCH -> (srcSpec, srcTaskDef)) ) )
       
       val branchInfo = new BranchInfo(Task.NO_BRANCH, grafts)
       if (srcTaskDef != taskDef) { // don't create cycles
         recordParentsFunc(branchInfo, Some(srcTaskDef))
       } else {
         recordParentsFunc(branchInfo, None)
       }
       branchPoints += Task.NO_BRANCH_POINT
       branchPointsByTask.getOrElseUpdate(taskDef, {new mutable.HashSet}) += Task.NO_BRANCH_POINT  
     }
     
     def generateBranchSpecs(bpName: String, start: BigDecimal, end: BigDecimal, inc: BigDecimal): Seq[LiteralSpec] = {
       for (value <- start.to(end, step=inc)) yield {
         new LiteralSpec(bpName.toLowerCase + value.toString, new Literal(value.toString), dotVariable=false)
       }
     }

     inSpec.rval match {
       case BranchPointDef(branchPointNameOpt, branchSpecz) => {
         val branchSpecs = branchSpecz
         val branchPointName: String = branchPointNameOpt match {
           case Some(name) => name
           case None => throw new FileFormatException("Branch point name is required", inSpec)
         }
         handleBranchPoint(branchPointName, branchSpecs)
       }
       case SequentialBranchPoint(branchPointNameOpt: Option[_], 
                                  sequence: Sequence) => {
         val branchPointName: String = branchPointNameOpt match {
           case Some(name) => name
           case None => throw new FileFormatException("Branch point name is required", inSpec)
         }
         val branchSpecs = generateBranchSpecs(branchPointName, sequence.start, sequence.end, sequence.increment)
         handleBranchPoint(branchPointName, branchSpecs, Some(CONFIG_TASK_DEF))
       }
       case ConfigVariable(varName) => {
         // config variables can also introduce branch points...
         // TODO: Can config variables be recursive?
         confSpecs.get(varName) match {
           case Some(confSpec) => {
             confSpec.rval match {
               case BranchPointDef(branchPointNameOpt, branchSpecz) => {
                  val branchSpecs: Seq[Spec] = branchSpecz
                  val branchPointName: String = branchPointNameOpt match {
                   case Some(name) => name
                   case None => throw new FileFormatException("Branch point name is required", inSpec)
                 }
                 // XXX: Some(CONFIG_TASK_DEF) is a nasty hack
                 handleBranchPoint(branchPointName, branchSpecs, Some(CONFIG_TASK_DEF))
               }
               case SequentialBranchPoint(branchPointNameOpt: Option[_], 
                                          sequence: Sequence) => {
                 val branchPointName: String = branchPointNameOpt match {
                   case Some(name) => name
                   case None => throw new FileFormatException("Branch point name is required", inSpec)
                 }
                 val branchSpecs = generateBranchSpecs(branchPointName, sequence.start, sequence.end, sequence.increment)
                 handleBranchPoint(branchPointName, branchSpecs, Some(CONFIG_TASK_DEF))
               }
               case ConfigVariable(_) => {
                 throw new FileFormatException(
                   "Recursive config variable %s required by input %s at task %s is not yet supported by ducttape".
                     format(varName, inSpec.name, taskDef.name),
                   List(inSpec, confSpec))
               }
               case _ => handleNonBranchPoint()
             }
           }
           case None => throw new FileFormatException(
             "Config variable %s required by input %s at task %s not found in config file.".
               format(varName, inSpec.name, taskDef.name),
             inSpec)
         }
       }
       case _ => handleNonBranchPoint()
     }
   }
}