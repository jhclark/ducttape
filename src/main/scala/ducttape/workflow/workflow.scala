package ducttape.workflow

import collection._

import ducttape.hyperdag._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.FileFormatException
import ducttape.versioner._
import ducttape.Types._

// TODO: Move into HyperDAG?
class Realization(val branches: Seq[Branch]) {
 // TODO: Keep string branch point names?

  // sort by branch *point* names to keep ordering consistent, then join branch names using dashes
  // and don't include our default branch "baseline"
  private def realizationName(real: Map[String,Branch]): String = {
    val branches = real.toSeq.sortBy(_._1).map(_._2)
    val names = branches.map(_.name).filter(_ != Task.NO_BRANCH.name)
    // TODO: Can we get rid of some of these cases now?
    names.size match {
      case 0 => Task.NO_BRANCH.name // make sure we have at least baseline in the name
      case _ => names.mkString("-")
    }
  }

  //def realizationName(real: Seq[Branch]) = realizationName(branchesToMap(real))
  private def branchesToMap(real: Seq[Branch]) = {
    val result = new mutable.HashMap[String,Branch]
    result += Task.NO_BRANCH_POINT.name -> Task.NO_BRANCH // TODO: XXX: Should we enforce this elsewhere?
    for(branch <- real) {
      result += branch.branchPoint.name -> branch
    }
    result
  }

  lazy val activeBranchMap = branchesToMap(branches)
  lazy val str = realizationName(activeBranchMap)

  override def hashCode = str.hashCode // TODO: More efficient?
  override def equals(obj: Any) = obj match { case that: Realization => this.str == that.str } // TODO: More efficient?
  override def toString = str
}

// short for "realized task"
// we might shorten this to Task
class RealTask(val taskT: TaskTemplate,
               val realization: Realization,
               // TODO: Change inputVals and paramVals over to Realization?
               val inputVals: Seq[(Spec,Spec,TaskDef,Seq[Branch])], // (mySpec,srcSpec,srcTaskDef,srcRealization)
               val paramVals: Seq[(Spec,LiteralSpec,TaskDef,Seq[Branch])], // (mySpec,srcSpec,srcTaskDef,srcRealization)
               val version: Int) { // workflow version
   def name = taskT.name
   def taskDef = taskT.taskDef
   def comments = taskT.comments
   def inputs = taskT.inputs
   def outputs = taskT.outputs
   def params = taskT.params
   def commands = taskT.commands // TODO: This will no longer be valid once we add in-lines

  // the tasks and realizations that must temporally precede this task (due to having required input files)
   lazy val antecedents: Set[(String, Realization)] = {
     for( (inSpec, srcSpec, srcTaskDef, srcRealization) <- inputVals) yield {
       (srcTaskDef.name, new Realization(srcRealization)) // TODO: change seq[branch] to realization?
     }
   }.filter{case (srcTaskDefName, _) => srcTaskDefName != WorkflowBuilder.CONFIG_TASK_DEF.name }.toSet

  // TODO: Smear hash code better
   override def hashCode = name.hashCode ^ realization.hashCode ^ version
   override def equals(obj: Any) = obj match {
     case that: RealTask => this.name == that.name && this.realization == that.realization && this.version == that.version
   }

   override def toString = "%s/%s".format(name, realization.toString)
 }

 // a TaskTemplate is a TaskDef with its input vals, param vals, and branch points resolved
 // TODO: fix these insane types for inputVals and paramVals
 class TaskTemplate(val taskDef: TaskDef,
            val branchPoints: Seq[BranchPoint], // only the branch points introduced at this task
            val inputVals: Seq[(Spec,Map[Branch,(Spec,TaskDef)])], // (mySpec,srcSpec,srcTaskDef)
            val paramVals: Seq[(Spec,Map[Branch,(LiteralSpec,TaskDef)])] ) { // (mySpec,srcSpec,srcTaskDef)
   def name = taskDef.name
   def comments = taskDef.comments
   def inputs = taskDef.inputs
   def outputs = taskDef.outputs
   def params = taskDef.params
   def commands = taskDef.commands

   override def toString = name

   // realize this task by specifying one branch per branch point
   // activeBranches should contain only the hyperedges encountered up until this vertex
   // with the key being the branchPointNames
   def realize(v: UnpackedWorkVert, versions: WorkflowVersioner): RealTask = {
     // TODO: Assert all of our branch points are satisfied
     // TODO: We could try this as a view.map() instead of just map() to only calculate these on demand...
     val realization = new Realization(v.realization)
     val activeBranchMap = realization.activeBranchMap

     // do a bit of sanity checking
     for(branchPoint <- branchPoints) {
       assert(activeBranchMap.contains(branchPoint.name),
              "Required branch point for this task '%s' not found in active branch points '%s'"
              .format(branchPoint.name, activeBranchMap.keys.mkString("-")))
     }

     // iterate over the hyperedges selected in this realization
     // remember: *every* metaedge has exactly one active incoming hyperedge
     val spec2reals = new mutable.HashMap[Spec, Seq[Branch]]
     for( (he: HyperEdge[Branch, Seq[Spec]], parentRealsByE: Seq[Seq[Branch]])
          <- v.edges.zip(v.parentRealizations)) {
       val edges = he.e.zip(parentRealsByE).filter{case (e, eReals) => e != null}
       for( (specs: Seq[Spec], srcReal: Seq[Branch]) <- edges) {
         for(spec <- specs) {
           //System.err.println("Spec %s has source real: %s".format(spec, srcReal))
           spec2reals += spec -> srcReal
         }
       }
     }

     // TODO: So how on earth are all these things parallel to meta edges etc?
     // TODO: XXX: What about a branch point that internally points to a config line that also has a branch point?

     def mapVal[T <: Spec](origSpec: Spec, curSpec: Spec, branchMap: Map[Branch,(T,TaskDef)]): (Spec,T,TaskDef,Seq[Branch]) = {
       curSpec.rval match {
         case BranchPointDef(branchPointName, _) => {
           val activeBranch: Branch = activeBranchMap(branchPointName)
           val(srcSpec: T, srcTaskDef) = branchMap(activeBranch)
           // TODO: Borken for params
           val parentReal = spec2reals(origSpec)
           (origSpec, srcSpec, srcTaskDef, parentReal)
         }
         case ConfigVariable(_) => {
           val(srcSpec: T, srcTaskDef) = branchMap.values.head
           // config variables can, in turn, define branch points, so we must be recursive
           mapVal(origSpec, srcSpec, branchMap)
         }
         case Variable(_,_) => { // not a branch point, but defined elsewhere
           val(srcSpec: T, srcTaskDef) = branchMap.values.head
           val parentReal = spec2reals(origSpec)
           (origSpec, srcSpec, srcTaskDef, parentReal)
         }
         case _ => { // not a branch point, but either a literal or unbound
           val(srcSpec: T, srcTaskDef) = branchMap.values.head
           (origSpec, srcSpec, srcTaskDef, v.realization)
         }
       }
     }
     
     // resolve the source spec/task for the selected branch
     def mapVals[T <: Spec](values: Seq[(Spec,Map[Branch,(T,TaskDef)])]): Seq[(Spec,T,TaskDef,Seq[Branch])] = {
       values.map{ case (mySpec: Spec, branchMap: Map[Branch, (T,TaskDef)]) => mapVal(mySpec, mySpec, branchMap) }
     }

     val realInputVals = mapVals(inputVals)
     val realParamVals = mapVals(paramVals)

     val version = versions(taskDef.name, realization)
     new RealTask(this, realization, realInputVals, realParamVals, version)
   }
 }

 // TODO: ???
 class UnpackState {
   
 }

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
         case Variable(srcTaskName, srcOutName) => {
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
   def build(wd: WorkflowDefinition, configSpecs: Seq[Spec]): HyperWorkflow = {

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

         def handleBranchPoint(branchPointName: String, branchSpecs: Seq[Spec]) = {
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
               if(srcTaskDef != taskDef) { // don't create cycles
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
                     handleBranchPoint(branchPointName, branchSpecs)
                   }
                   case ConfigVariable(_) => {
                     throw new FileFormatException(
                       "Recursive config variable %s required by input %s at task %s is not yet supported by ducttape".format(
                         varName, inSpec.name, taskDef.name),
                       List( (wd.file, inSpec.pos, inSpec.pos.line), (wd.file, confSpec.pos, confSpec.pos.line) ))
                   }
                   case _ => {
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

     // == we've just completed our second pass over the workflow file and linked everything together ==

     // now build a graph representation by adding converting to (meta/hyper) edges
     for(v <- vertices.values) {
       val task: TaskTemplate = v.value

       // add one metaedge per branch point
       for(branchPoint <- branchPointsByTask(task.taskDef)) {
         // create a hyperedge list in the format expected by the HyperDAG API

         val hyperedges = new mutable.ArrayBuffer[(Branch, Seq[(Option[PackedVertex[TaskTemplate]],Seq[Spec])])]
         for( (branch, parentTaskDefs) <- parents(task); if branchPoint == branch.branchPoint) {
           val edges = new mutable.ArrayBuffer[(Option[PackedVertex[TaskTemplate]],Seq[Spec])]

           // find which inputs and parameters are attached to this branch point
           // this will be the payload associated with each plain edge in the MetaHyperDAG
           def findInputParamSpecs(parentTaskDef: TaskDef): Seq[Spec] = {
               (task.inputVals ++ task.paramVals).filter{
                 case (ipSpec: Spec, specBranches: Map[Branch,(Spec,TaskDef)]) => {
                   specBranches.get(branch) match {
                     case None => false
                     case Some( (spec: Spec, specParent: TaskDef) ) => specParent == parentTaskDef
                   }
                 }
               }.map{ case(ipSpec, specBranches) => ipSpec }
           }

           // parents are stored as Options so that we can use None to indicate phantom parent vertices
           for( parentTaskDefOpt: Option[TaskDef] <- parentTaskDefs) parentTaskDefOpt match {
             case Some(CONFIG_TASK_DEF) => ; // no edge implied here
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
}
