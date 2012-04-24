package ducttape.workflow

import collection._
import ducttape.hyperdag._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.FileFormatException
import ducttape.versioner._
import ducttape.workflow.Types._
import ducttape.hyperdag.meta.MetaHyperDag
import ducttape.hyperdag.meta.MetaHyperDagBuilder
import ducttape.syntax.BashCode

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
class WorkflowBuilder(wd: WorkflowDefinition, configSpecs: Seq[ConfigAssignment], builtins: Seq[WorkflowDefinition]) {
  
  import WorkflowBuilder._
  
   val branchPointFactory = new BranchPointFactory
   val branchFactory = new BranchFactory(branchPointFactory)
   val dag = new MetaHyperDagBuilder[TaskTemplate,BranchPoint,BranchInfo,Seq[Spec]]
  
  
  private def findTasks(wd: WorkflowDefinition,
                        confSpecs: Map[String,Spec],
                        defMap: mutable.HashMap[String,TaskDef],
                        parents: mutable.HashMap[TaskTemplate,Map[BranchInfo,mutable.Set[Option[TaskDef]]]],
                        vertices: mutable.HashMap[String,ducttape.hyperdag.PackedVertex[TaskTemplate]],
                        branchPoints: mutable.ArrayBuffer[BranchPoint],
                        branchPointsByTask: mutable.HashMap[TaskDef,mutable.Set[BranchPoint]]) {
    
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
       for (paramSpec: Spec <-taskDef.params) {
         def recordParentsFunc(branchInfo: BranchInfo, srcTaskDef: Option[TaskDef]) {
           // params have no effect on temporal ordering, but can affect derivation of branches
           // therefore, params are *always* rooted at phantom vertices, no matter what
          parentsByBranch.getOrElseUpdate(branchInfo, {new mutable.HashSet}) += None
         }
         def resolveVarFunc(taskDef: TaskDef, defMap: Map[String,TaskDef], paramSpec: Spec)
           : (LiteralSpec, TaskDef, Seq[Branch]) = {
             resolveParam(wd, confSpecs, taskDef, defMap, paramSpec)
         }
         resolveBranchPoint(taskDef, paramSpec, defMap, confSpecs, branchPoints, branchPointsByTask,
                            paramVals, recordParentsFunc, resolveVarFunc)
       }

       val inputVals = new mutable.ArrayBuffer[(Spec,Map[Branch,(Spec,TaskDef)])](taskDef.inputs.size)
       // TODO: Roll own multimap since scala's is a bit awkward
       for (inSpec: Spec <- taskDef.inputs) {
         def recordParentsFunc(branchInfo: BranchInfo, srcTaskDef: Option[TaskDef]) {
           // make a note of what edges we'll need to add later
           parentsByBranch.getOrElseUpdate(branchInfo, {new mutable.HashSet}) += srcTaskDef
         }
         def resolveVarFunc(taskDef: TaskDef, defMap: Map[String,TaskDef], inSpec: Spec)
           : (Spec, TaskDef, Seq[Branch]) = {
             resolveVar(wd, confSpecs, taskDef, defMap, inSpec, InputMode())
         }
         resolveBranchPoint(taskDef, inSpec, defMap, confSpecs, branchPoints, branchPointsByTask,
                            inputVals, recordParentsFunc, resolveVarFunc)
       }

       if (paramVals.size == 0 && inputVals.size == 0) {
         // we MUST inherit from at least the baseline branch point
         branchPointsByTask.getOrElseUpdate(taskDef, {new mutable.HashSet}) += Task.NO_BRANCH_POINT
       }

       val task = new TaskTemplate(taskDef, branchPointsByTask(taskDef).toSeq, inputVals, paramVals)
       parents += task -> parentsByBranch
       vertices += task.name -> dag.addVertex(task)
     }
   }

   // the resolved Spec is guaranteed to be a literal for params
   private def resolveParam(wd: WorkflowDefinition,
                            confSpecs: Map[String,Spec],
                            taskDef: TaskDef,
                            map: Map[String,TaskDef],
                            spec: Spec)
   : (LiteralSpec, TaskDef, Seq[Branch]) = {
     resolveVar(wd, confSpecs, taskDef, map, spec, ParamMode()).asInstanceOf[(LiteralSpec,TaskDef,Seq[Branch])]
   }

   // TODO: document what's going on here -- maybe move elsewhere
   private def resolveVar(wd: WorkflowDefinition,
                          confSpecs: Map[String,Spec],
                          taskDef: TaskDef,
                          map: Map[String,TaskDef],
                          spec: Spec,
                          mode: ResolveMode)
   : (Spec, TaskDef, Seq[Branch]) = {

     var curSpec: Spec = spec
     var src: TaskDef = taskDef
     var grafts: Seq[Branch] = Nil
     
     def handleTaskVar(srcTaskName: String, srcOutName: String) {
      map.get(srcTaskName) match {
       case Some(srcDef: TaskDef) => {
         // determine where to search when resolving this variable's parent spec
         val specSet = mode match {
           case InputMode() => srcDef.outputs
           case ParamMode() => srcDef.params
         }
         // now search for the parent spec
         specSet.find(outSpec => outSpec.name == srcOutName) match {
           case Some(srcSpec) => curSpec = srcSpec
           case None => {
             throw new FileFormatException(
               "Output %s at source task %s for required by input %s at task %s not found. Candidate outputs are: %s".
               format(srcOutName, srcTaskName, spec.name, taskDef.name, specSet.map(_.name).mkString(" ")),
               List(spec, srcDef))
           }
         }
         // assign after we've gotten a chance to print error messages
         src = srcDef
       }
       case None => {
         throw new FileFormatException(
           "Source task %s for input %s at task %s not found".
           format(srcTaskName, spec.name, taskDef.name), spec)
       }
      }
     }
     
     while (true) {
       curSpec.rval match {
         case Literal(litValue) => {
           // TODO: can we enforce this as part of the match instead of using a cast?
           val litSpec = curSpec.asInstanceOf[LiteralSpec] // guaranteed to succeed
           return (litSpec, src, Nil) // literals will never have a use for grafts
         }
         case ConfigVariable(varName) => {
           confSpecs.get(varName) match {
             // TODO: Does this TaskDef break line numbering for error reporting?
             // TODO: Should we return? Or do we allow config files to point back into workflows?
             case Some(confSpec) => return (confSpec, CONFIG_TASK_DEF, grafts)
             case None => throw new FileFormatException(
                 "Config variable %s required by input %s at task %s not found in config file.".
                   format(varName, spec.name, taskDef.name),
                   List(spec, src))
           }
         }
         case TaskVariable(srcTaskName, srcOutName) => {
           handleTaskVar(srcTaskName, srcOutName)
         }
         case BranchGraft(srcOutName, srcTaskName, branchGraftElements) => {
           handleTaskVar(srcTaskName, srcOutName)
           grafts = branchGraftElements.map{e => branchFactory(e.branchName, e.branchPointName) }
         }
         case BranchPointDef(_,_) => throw new RuntimeException("Expected branches to be resolved by now")
         case SequentialBranchPoint(_,_,_,_) => throw new RuntimeException("Expected sequences to be resolved by now")
         case Unbound() => {
           mode match {
             case InputMode() => {
               // make sure we didn't just refer to ourselves -- 
               // referring to an unbound output is fine though (and usual)
               if(src != taskDef || spec != curSpec) {
                 return (curSpec, src, grafts)
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
     throw new Error("Unreachable")
   }

   // create dependency pointers based on workflow definition
   // TODO: This method has become morbidly obese -- break it out into several methods
   def build(): HyperWorkflow = {

     val confSpecs: Map[String, Spec] = configSpecs.map{ass => (ass.spec.name, ass.spec)}.toMap
     val defMap = new mutable.HashMap[String,TaskDef]
     for (t: TaskDef <- wd.tasks) {
       defMap += t.name -> t
     }

     // (task, parents) -- Option as None indicates that parent should be a phantom vertex
     // so as not to affect temporal ordering nor appear when walking the DAG
     // TODO: Fix this painful data structure into something more readable (use typedefs?)
     val parents = new mutable.HashMap[TaskTemplate, Map[BranchInfo,mutable.Set[Option[TaskDef]]]] // TODO: Multimap
     val vertices = new mutable.HashMap[String,PackedVertex[TaskTemplate]]

     val branchPoints = new mutable.ArrayBuffer[BranchPoint]
     val branchPointsByTask = new mutable.HashMap[TaskDef,mutable.Set[BranchPoint]] // TODO: Multimap
     
     // first, create tasks, but don't link them in graph yet
     findTasks(wd, confSpecs, defMap, parents, vertices, branchPoints, branchPointsByTask)

     // == we've just completed our first pass over the workflow file and linked everything together ==

     // now build a graph representation by adding converting to (meta/hyper) edges
     for (v <- vertices.values) {
       val task: TaskTemplate = v.value
       
       //System.err.println("Adding %s to HyperDAG".format(task))

       // add one metaedge per branch point
       // the Baseline branch point and baseline branch are automatically added by findTasks() in the first pass
       for (branchPoint <- branchPointsByTask(task.taskDef)) {
 
         // create a hyperedge list in the format expected by the HyperDAG API
         val hyperedges = new mutable.ArrayBuffer[(BranchInfo, Seq[(Option[PackedVertex[TaskTemplate]],Seq[Spec])])]
         for ( (branchInfo, parentTaskDefs) <- parents(task); if branchPoint == branchInfo.branch.branchPoint) {

           // create an edge within each hyperedge for each input associated with a task
           // so that we will know which realization to use at the source of each edge.
           // parameters may also be subject to branching, but will never point at a task directly
           // since parameters do not imply a temporal ordering among task vertices.
           val edges = new mutable.ArrayBuffer[(Option[PackedVertex[TaskTemplate]],Seq[Spec])]
           val antiedges = new mutable.ArrayBuffer[(Option[PackedVertex[TaskTemplate]],Seq[Spec])]

           // find which inputs are attached to this branch point
           // unlike params (which are just lumped into an edge to a single phantom vertex),
           // we must match the parent task so that we attach to the correct hyperedge
           // this will be the payload associated with each plain edge in the MetaHyperDAG
           def findInputSpecs(parentTaskDef: TaskDef): Seq[Spec] = {
               task.inputVals.filter{
                 case (ipSpec, map) => {
                   val specBranches: Map[Branch,(Spec,TaskDef)] = map
                   specBranches.get(branchInfo.branch) match {
                     case None => false
                     case Some( (_, specParent: TaskDef) ) => {
                       //System.err.println("Comparing: %s %s %s".format(specParent, parentTaskDef, specParent == parentTaskDef))
                       specParent == parentTaskDef
                     }
                   }
                 }
               }.map{
                 case (ipSpec, specBranches) => ipSpec
               }
           }
           
           // see notes for findInputSpecs()
           // we need only filter by branch since all params get lumped into a single phantom vertex
           // through a single hyperedge *per branch*
           def findParamSpecs(): Seq[Spec] = {
               task.paramVals.filter{
                 case (ipSpec, specBranchez) => {
                   val specBranches: Map[Branch,(Spec,TaskDef)] = specBranchez
                   specBranches.contains(branchInfo.branch)
                 }
               }.map{
                 case (ipSpec, specBranches) => ipSpec
               }
           }

           // parents are stored as Options so that we can use None to indicate phantom parent vertices
           parentTaskDefs.foreach {
             case Some(CONFIG_TASK_DEF) => {
               // this includes only paths (not params) defined in a config
               //
               // config entries may in turn contain branches, which require
               // edges to reconstruct which inputs/parameters each realized task should use
               // the parent task *of the ConfigVariable* will be listed as the current task
               val ipSpecs = findInputSpecs(task.taskDef)
               //System.err.println("CURRENT BRANCH %s HAS IP SPECS FOR CONFIG: %s".format(branchInfo, ipSpecs.toList))
               //System.err.println("INPUT VALS: " + task.inputVals)
               //System.err.println("Found config task def only")
               edges.append( (None, ipSpecs) )
             }
             case Some(parentTaskDef) => {
               val parentVert = vertices(parentTaskDef.name)
               val ipSpecs = findInputSpecs(parentTaskDef)
               // add an edge for each parameter/input at task that originates from parentVert
               //System.err.println("IP specs for branch point %s are: %s".format(branchPoint, ipSpecs))
               edges.append( (Some(parentVert), ipSpecs) )
             }
             case None => {
               // We have a branch point (possibly baseline) using one of the following:
               // 1) literal paths
               // 2) any kind of param values (since params never induce temporal dependencies)
               //
               // however, params may still need to be statically resolved later, which is why
               // we use findInputParamSpecs(). unlike literal paths/params, 
               //System.err.println("Found literal paths or some type of params only")
               val ipSpecs = findParamSpecs() ++ findInputSpecs(task.taskDef) // we are our own parent
               edges.append( (None, ipSpecs) )
             }
           }
           hyperedges.append( (branchInfo, edges) )
        }
        if (!hyperedges.isEmpty) {
          // NOTE: The meta edges are not necessarily phantom, but just have that option
          //System.err.println("Adding metaedge for branchPoint %s task %s to HyperDAG: Component hyperedges are: %s".format(branchPoint, task, hyperedges))
          dag.addPhantomMetaEdge(branchPoint, hyperedges, v)
        } else {
          //System.err.println("No metaedge for branchPoint %s at task %s is needed (zero component hyperedges)".format(branchPoint, task))
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
   
  def buildPlans(planDefs: Seq[PlanDefinition]): Seq[RealizationPlan] = {
    val result = new mutable.ArrayBuffer[RealizationPlan]
    
    for (planDef: PlanDefinition <- planDefs) {
      for (cross: CrossProduct <- planDef.crossProducts) {
        val realizations = new mutable.HashMap[BranchPoint, Set[Branch]]
        for (ref: BranchPointRef <- cross.value) {
          try {
            val branchPoint: BranchPoint = branchPointFactory(ref.name)
            val branches: Set[Branch] = ref.branchNames.map(name => branchFactory(name, branchPoint)).toSet
            realizations += branchPoint -> branches
          } catch {
            // TODO: Move to err2exception?
            case e: NoSuchBranchPointException => {
              Console.err.println("ERROR: No such branch point: %s".format(e.msg))
              sys.exit(1)
            }
            case e: NoSuchBranchException => {
              Console.err.println("ERROR: No such branch: %s".format(e.msg))
              sys.exit(1)
            }
          }
        }
        result += new RealizationPlan(planDef.name, cross.goals, realizations)
      }
    }
    result
  }
   
   // define branch resolution as a function that takes some callback functions
   // since we use it for both parameter and input file resolution,
   // but it behaves slightly different for each (parameters don't
   // imply a temporal ordering between vertices)
   def resolveBranchPoint[SpecT](taskDef: TaskDef,
                                 inSpec: Spec,
                                 defMap: Map[String,TaskDef],
                                 confSpecs: Map[String,Spec],
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
       val result = new mutable.ArrayBuffer[LiteralSpec]
       var value = start
       while (value <= end) {
         result += new LiteralSpec(bpName.toLowerCase + value.toString, new Literal(value.toString), dotVariable=false)
         value += inc
       }
       result
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
       case SequentialBranchPoint(branchPointNameOpt, start: BigDecimal, end: BigDecimal, increment: BigDecimal) => {
         val branchPointName: String = branchPointNameOpt match {
           case Some(name) => name
           case None => throw new FileFormatException("Branch point name is required", inSpec)
         }
         val branchSpecs = generateBranchSpecs(branchPointName, start, end, increment)
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
               case SequentialBranchPoint(branchPointNameOpt, start: BigDecimal, end: BigDecimal, increment: BigDecimal) => {
                 val branchPointName: String = branchPointNameOpt match {
                   case Some(name) => name
                   case None => throw new FileFormatException("Branch point name is required", inSpec)
                 }
                 val branchSpecs = generateBranchSpecs(branchPointName, start, end, increment)
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
