package ducttape.workflow

import collection._
import ducttape.hyperdag._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.FileFormatException
import ducttape.versioner._
import ducttape.workflow.Types._
import ducttape.hyperdag.meta._

/**
 * This is where the real magic happens of turning an Abstract Syntax Tree
 * into an immutable HyperWorkflow that everything else can use to perform actions.
 */
object WorkflowBuilder {

   // TODO: Better error reporting here?
   val CONFIG_TASK_DEF = new TaskDef("CONFIGURATION_FILE", new CommentBlock(Nil), Nil, Nil, Nil, Nil, scala.util.parsing.input.NoPosition)

   class ResolveMode();
   case class InputMode() extends ResolveMode;
   case class ParamMode() extends ResolveMode;
   case class OutputMode() extends ResolveMode;

   // the resolved Spec is guaranteed to be a literal for params
   private def resolveParam(wd: WorkflowDefinition,
                            confSpecs: Map[String,Spec],
                            taskDef: TaskDef,
                            map: Map[String,TaskDef],
                            spec: Spec)
   : (LiteralSpec, TaskDef) = {
     resolveVar(wd, confSpecs, taskDef, map, spec, ParamMode()).asInstanceOf[(LiteralSpec,TaskDef)]
   }

   // TODO: document what's going on here -- maybe move elsewhere
   private def resolveVar(wd: WorkflowDefinition,
                          confSpecs: Map[String,Spec],
                          taskDef: TaskDef,
                          map: Map[String,TaskDef],
                          spec: Spec,
                          mode: ResolveMode)
   : (Spec, TaskDef) = {

     var curSpec: Spec = spec
     var src: TaskDef = taskDef
     while(true) {
       curSpec.rval match {
         case Literal(litValue) => {
           // TODO: can we enforce this as part of the match instead of using a cast?
           val litSpec = curSpec.asInstanceOf[LiteralSpec] // guaranteed to succeed
           return (litSpec, src)
         }
         case ConfigVariable(varName) => {
           confSpecs.get(varName) match {
             // TODO: Does this TaskDef break line numbering for error reporting?
             // TODO: Should we return? Or do we allow config files to point back into workflows?
             case Some(confSpec: Spec) => return (confSpec, CONFIG_TASK_DEF)
             case None => {
               throw new FileFormatException(
                 "Config variable %s required by input %s at task %s not found in config file.".format(
                   varName, spec.name, taskDef.name),
                 List( (wd.file, spec.pos, spec.pos.line), (wd.file, src.pos, src.lastHeaderLine) ))
             }
           }
         }
         case TaskVariable(srcTaskName, srcOutName) => {
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
                     "Output %s at source task %s for required by input %s at task %s not found. Candidate outputs are: %s".format(
                       srcOutName, srcTaskName, spec.name, taskDef.name, specSet.map(_.name).mkString(" ")),
                     List( (wd.file, spec.pos, spec.pos.line), (wd.file, srcDef.pos, srcDef.lastHeaderLine) ))
                 }
               }
               // assign after we've gotten a chance to print error messages
               src = srcDef
             }
             case None => {
               throw new FileFormatException(
                 "Source task %s for input %s at task %s not found".format(
                   srcTaskName, spec.name, taskDef.name), wd.file, spec.pos)
             }
           }
         }
         case BranchPointDef(name, specs: Seq[Spec]) => {
           throw new RuntimeException("Expected branches to be resolved by now")
         }
         case Unbound() => {
           mode match {
             case InputMode() => {
               // make sure we didn't just refer to ourselves -- 
               // referring to an unbound output is fine though (and usual)
               if(src != taskDef || spec != curSpec) {
                 return (curSpec, src)
               } else {
                 throw new FileFormatException("Unbound input variable: %s".format(curSpec.name),
                                               List((wd.file, taskDef.pos), (wd.file, src.pos)))
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
   def build(wd: WorkflowDefinition, configSpecs: Seq[ConfigAssignment]): HyperWorkflow = {

     val confSpecs: Map[String, Spec] = configSpecs.map{spec => (spec.name, spec)}.toMap
     val defMap = new mutable.HashMap[String,TaskDef]
     for(t <- wd.tasks) {
       defMap += t.name -> t
     }

     // (task, parents) -- Option as None indicates that parent should be a phantom vertex
     // so as not to affect temporal ordering nor appear when walking the DAG
     // TODO: Fix this painful data structure into something more readable (use typedefs?)
     val parents = new mutable.HashMap[TaskTemplate, Map[Branch,mutable.Set[Option[TaskDef]]]] // TODO: Multimap
     val vertices = new mutable.HashMap[String,PackedVertex[TaskTemplate]]
     val dag = new MetaHyperDagBuilder[TaskTemplate,BranchPoint,Branch,Seq[Spec]]

     val branchPointFactory = new BranchPointFactory
     val branchFactory = new BranchFactory(branchPointFactory)
     val branchPoints = new mutable.ArrayBuffer[BranchPoint]
     val branchPointsByTask = new mutable.HashMap[TaskDef,mutable.Set[BranchPoint]] // TODO: Multimap
     
     // first, create tasks, but don't link them in graph yet
     findTasks(wd, confSpecs, defMap, parents, vertices, dag, branchPointFactory, branchFactory, branchPoints, branchPointsByTask)

     // == we've just completed our second pass over the workflow file and linked everything together ==

     // now build a graph representation by adding converting to (meta/hyper) edges
     for(v <- vertices.values) {
       val task: TaskTemplate = v.value

       // add one metaedge per branch point
       for(branchPoint <- branchPointsByTask(task.taskDef)) {

         // create a hyperedge list in the format expected by the HyperDAG API
         val hyperedges = new mutable.ArrayBuffer[(Branch, Seq[(Option[PackedVertex[TaskTemplate]],Seq[Spec])])]
         for( (branch, parentTaskDefs) <- parents(task); if branchPoint == branch.branchPoint) {

           // create an edge within each hyperedge for each input associated with a task
           // so that we will know which realization to use at the source of each edge.
           // parameters may also be subject to branching, but will never point at a task directly
           // since parameters do not imply a temporal ordering among task vertices.
           val edges = new mutable.ArrayBuffer[(Option[PackedVertex[TaskTemplate]],Seq[Spec])]

           // find which inputs and parameters are attached to this branch point
           // this will be the payload associated with each plain edge in the MetaHyperDAG
           def findInputParamSpecs(parentTaskDef: TaskDef): Seq[Spec] = {
               (task.inputVals ++ task.paramVals).filter{
                 case (ipSpec: Spec, specBranches: Map[Branch,(Spec,TaskDef)]) => {
                   specBranches.get(branch) match {
                     case None => false
                     case Some( (spec: Spec, specParent: TaskDef) ) => {
                       //System.err.println("Comparing: %s %s".format(specParent, parentTaskDef))
                       specParent == parentTaskDef
                     }
                   }
                 }
               }.map{ case(ipSpec, specBranches) => ipSpec }
           }

           // parents are stored as Options so that we can use None to indicate phantom parent vertices
           for( parentTaskDefOpt: Option[TaskDef] <- parentTaskDefs) parentTaskDefOpt match {
             case Some(CONFIG_TASK_DEF) => {
               // config entries may in turn contain branches, which require
               // edges to reconstruct which inputs/parameters each realized task should use
               // the parent task *of the ConfigVariable* will be listed as the current task
               val ipSpecs = findInputParamSpecs(task.taskDef)
               //System.err.println("CURRENT BRANCH %s HAS IP SPECS FOR CONFIG: %s".format(branch, ipSpecs.toList))
               //System.err.println("INPUT VALS: " + task.inputVals)
               edges.append( (None, ipSpecs) )
             }
             case Some(parentTaskDef) => {
               val parentVert = vertices(parentTaskDef.name)
               val ipSpecs = findInputParamSpecs(parentTaskDef)
               // add an edge for each parameter/input at task that originates from parentVert
               //System.err.println("IP specs for branch point %s are: %s".format(branchPoint, ipSpecs))
               edges.append( (Some(parentVert), ipSpecs) )
             }
             case None => {
               // We have a branch point using only literal paths
               // or literal param values
               val ipSpecs = findInputParamSpecs(task.taskDef) // we are our own parent
               edges.append( (None, ipSpecs) )
             }
           }
           hyperedges.append( (branch, edges) )
        }
        if(!hyperedges.isEmpty) {
          // NOTE: The meta edges are not necessarily phantom, but just have that option
          dag.addPhantomMetaEdge(branchPoint, hyperedges, v)
        }
      }
    }

    // TODO: For params, we can resolve these values *ahead*
    // of time, prior to scheduling (but keep relationship info around)
    // (i.e. parameter dependencies should not imply temporal dependencies)
    new HyperWorkflow(dag.build, branchPointFactory, branchFactory)
  }
  
  private def findTasks(wd: WorkflowDefinition,
					    confSpecs: Map[String,Spec],
					    defMap: mutable.HashMap[String,TaskDef],
					    parents: mutable.HashMap[TaskTemplate,Map[Branch,mutable.Set[Option[TaskDef]]]],
					    vertices: mutable.HashMap[String,ducttape.hyperdag.PackedVertex[TaskTemplate]],
					    dag: MetaHyperDagBuilder[TaskTemplate,BranchPoint,Branch,Seq[Spec]],
					    branchPointFactory: BranchPointFactory,
					    branchFactory: BranchFactory,
					    branchPoints: mutable.ArrayBuffer[BranchPoint],
					    branchPointsByTask: mutable.HashMap[TaskDef,mutable.Set[BranchPoint]]) {
    
     for(taskDef <- wd.tasks) {

       // TODO: Check for all inputs/outputs/param names being unique in this step
       if(vertices.contains(taskDef.name)) {
         val prev: TaskTemplate = vertices(taskDef.name).value
         throw new FileFormatException("Duplicate task name: %s".format(taskDef.name),
                                     List((wd.file, taskDef.pos), (wd.file, prev.taskDef.pos)))
       }

       // define branch resolution as a function that takes some callback functions
       // since we use it for both parameter and input file resolution,
       // but it behaves slightly different for each (parameters don't
       // imply a temporal ordering between vertices)
       def resolveBranchPoint[SpecT](inSpec: Spec,
                         confSpecs: Map[String, Spec],
                         resolvedVars: mutable.ArrayBuffer[(Spec,Map[Branch,(SpecT,TaskDef)])],
                         recordParentsFunc: (Branch,Option[TaskDef]) => Unit,
                         resolveVarFunc: (TaskDef, Map[String,TaskDef], Spec) => (SpecT, TaskDef) ) = {

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
             for(branchSpec <- branchSpecs) {
               val branch = branchFactory.get(branchSpec.name, branchPoint)
               val (srcSpec, srcTaskDef) = resolveVarFunc(taskDef, defMap, branchSpec)
               branchMap.put(branch, (srcSpec, srcTaskDef) )
               if(confForcedSrc != None) {
                 // XXX: Yes, we link this to 2 branches
                 recordParentsFunc(branch, confForcedSrc)
               } else if(srcTaskDef != taskDef) { // don't create cycles
                 recordParentsFunc(branch, Some(srcTaskDef))
               } else {
                 recordParentsFunc(branch, None)
               }
             }

             // TODO: Rework this so that branch points can be associated with multiple tasks?
             // Right now the constraintFilter is taking care of this at traversal time
             branchPoints += branchPoint
             branchPointsByTask.getOrElseUpdate(taskDef, {new mutable.HashSet}) += branchPoint
             resolvedVars.append( (inSpec, branchMap) )
         }

         def handleNonBranchPoint {
           val (srcSpec, srcTaskDef) = resolveVarFunc(taskDef, defMap, inSpec)
           resolvedVars.append( (inSpec, Map(Task.NO_BRANCH -> (srcSpec, srcTaskDef)) ) )
           
           if(srcTaskDef != taskDef) { // don't create cycles
             recordParentsFunc(Task.NO_BRANCH, Some(srcTaskDef))
           } else {
             recordParentsFunc(Task.NO_BRANCH, None)
           }
           branchPoints += Task.NO_BRANCH_POINT
           branchPointsByTask.getOrElseUpdate(taskDef, {new mutable.HashSet}) += Task.NO_BRANCH_POINT  
         }

         inSpec.rval match {
           case BranchPointDef(branchPointName, branchSpecs: Seq[Spec]) => {
             handleBranchPoint(branchPointName, branchSpecs)
           }
           case ConfigVariable(varName) => {
             // config variables can also introduce branch points...
             // TODO: Can config variables be recursive?
             confSpecs.get(varName) match {
               case Some(confSpec) => {
                 confSpec.rval match {
                   case BranchPointDef(branchPointName, branchSpecs: Seq[Spec]) => {
                     // XXX: Some(CONFIG_TASK_DEF) is a nasty hack
                     handleBranchPoint(branchPointName, branchSpecs, Some(CONFIG_TASK_DEF))
                   }
                   case ConfigVariable(_) => {
                     throw new FileFormatException(
                       "Recursive config variable %s required by input %s at task %s is not yet supported by ducttape".format(
                         varName, inSpec.name, taskDef.name),
                       List( (wd.file, inSpec.pos, inSpec.pos.line), (wd.file, confSpec.pos, confSpec.pos.line) ))
                   }
                   case _ => {
                     handleNonBranchPoint
                   }
                 }
               }
               case None => throw new FileFormatException(
                 "Config variable %s required by input %s at task %s not found in config file.".format(
                   varName, inSpec.name, taskDef.name),
                 List( (wd.file, inSpec.pos, inSpec.pos.line) ))
             }
           }
           case _ => {
             handleNonBranchPoint
           }
         }
       }

       val parentsByBranch = new mutable.HashMap[Branch,mutable.Set[Option[TaskDef]]]

       // parameters are different than file dependencies in that they do not
       // add any temporal dependencies between tasks and therefore do not
       // add any edges in the MetaHyperDAG
       val paramVals = new mutable.ArrayBuffer[(Spec,Map[Branch,(LiteralSpec,TaskDef)])](taskDef.params.size)
       for(paramSpec: Spec <-taskDef.params) {
         def recordParentsFunc(branch: Branch, srcTaskDef: Option[TaskDef]) {
           // params have no effect on temporal ordering, but can affect derivation of branches
           // therefore, params are *always* rooted at phantom vertices, no matter what
          parentsByBranch.getOrElseUpdate(branch, {new mutable.HashSet}) += None
         }
         def resolveVarFunc(taskDef: TaskDef, defMap: Map[String,TaskDef], paramSpec: Spec)
           : (LiteralSpec, TaskDef) = {
             resolveParam(wd, confSpecs, taskDef, defMap, paramSpec)
         }
         resolveBranchPoint(paramSpec, confSpecs, paramVals, recordParentsFunc, resolveVarFunc)
       }

       val inputVals = new mutable.ArrayBuffer[(Spec,Map[Branch,(Spec,TaskDef)])](taskDef.inputs.size)
       // TODO: Roll own multimap since scala's is a bit awkward
       for(inSpec: Spec <- taskDef.inputs) {
         def recordParentsFunc(branch: Branch, srcTaskDef: Option[TaskDef]) {
           // make a note of what edges we'll need to add later
           parentsByBranch.getOrElseUpdate(branch, {new mutable.HashSet}) += srcTaskDef
         }
         def resolveVarFunc(taskDef: TaskDef, defMap: Map[String,TaskDef], inSpec: Spec)
           : (Spec, TaskDef) = {
             resolveVar(wd, confSpecs, taskDef, defMap, inSpec, InputMode())
         }
         resolveBranchPoint(inSpec, confSpecs, inputVals, recordParentsFunc, resolveVarFunc)
       }

       if(paramVals.size == 0 && inputVals.size == 0) {
         // we MUST inherit from at least the baseline branch point
         branchPointsByTask.getOrElseUpdate(taskDef, {new mutable.HashSet}) += Task.NO_BRANCH_POINT
       }

       val task = new TaskTemplate(taskDef, branchPointsByTask(taskDef).toSeq, inputVals, paramVals)
       parents += task -> parentsByBranch
       vertices += task.name -> dag.addVertex(task)
     }
   }
}
