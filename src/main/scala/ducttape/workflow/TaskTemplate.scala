package ducttape.workflow

import collection._
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.Types._
import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
import ducttape.hyperdag.HyperEdge
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.syntax.FileFormatException
import ducttape.syntax.AbstractSyntaxTree.TaskVariable
import ducttape.workflow.SpecTypes._

/**
 * a TaskTemplate is a TaskDef with its input vals, param vals, and branch points resolved
 */
// TODO: fix these insane types for inputVals and paramVals
class TaskTemplate(val taskDef: TaskDef,
            val branchPoints: Seq[BranchPoint], // only the branch points introduced at this task
            val inputVals: Seq[(Spec,Map[Branch,(Spec,TaskDef)])],
            val paramVals: Seq[(Spec,Map[Branch,(LiteralSpec,TaskDef)])] ) { // (mySpec,srcSpec,srcTaskDef)
   def name = taskDef.name
   def comments = taskDef.comments
   def packages = taskDef.packages
   def inputs = taskDef.inputs
   def outputs = taskDef.outputs
   def params = taskDef.params
   def commands = taskDef.commands

   override def toString = name
   
   // NOTE: MEMORY WARNING: These realizations are not uniqued in any way. We might want to pool them at some point!

   // realize this task by specifying one branch per branch point
   // activeBranches should contain only the hyperedges encountered up until this vertex
   // with the key being the branchPointNames
   def realize(v: UnpackedWorkVert): RealTask = {
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
     // this annotation on the plain edges is created in WorkflowBuilder.build()
     val spec2reals = new mutable.HashMap[Spec, Realization]
     for( (heX, parentRealsByE: Seq[Seq[Branch]]) <- v.edges.zip(v.parentRealizations)) {
       val he: HyperEdge[Branch, Seq[Spec]] = heX
       val edges = he.e.zip(parentRealsByE).filter{case (e, eReals) => e != null}
       for( (specsX, srcRealX) <- edges) {
         val specs: Seq[Spec] = specsX
         val srcReal: Seq[Branch] = srcRealX
         for(spec <- specs) {
           //System.err.println("Spec %s has source real: %s".format(spec, srcReal))
           spec2reals += spec -> new Realization(srcReal) // TODO: Pool realizations?
         }
       }
     }

     // TODO: So how on earth are all these things parallel to meta edges etc?
     // TODO: XXX: What about a branch point that internally points to a config line that also has a branch point?

     // resolve the source spec/task for the selected branch
     // and return the 
     def mapVal[T <: Spec](origSpec: Spec, curSpec: Spec, branchMap: Map[Branch,(T,TaskDef)]): ResolvedSpecType[T] = {
       curSpec.rval match {
         case BranchPointDef(branchPointNameOpt, _) => {
           val branchPointName = branchPointNameOpt match {
             case Some(name) => name
             case None => throw new RuntimeException("Branch point name is required (this should have already been checked)")
           }
           val activeBranch: Branch = activeBranchMap(branchPointName)
           val (srcSpecX, srcTaskDef) = branchMap(activeBranch)
           val srcSpec: T = srcSpecX
           // TODO: Borken for params
           val parentReal = spec2reals(origSpec)
           new ResolvedSpecType[T](origSpec, srcSpec, srcTaskDef, parentReal)
         }
         case ConfigVariable(_) => {
           // config variables can, in turn, define branch points, so we must be careful
           // TODO: Borken for nested branch points
           val whichBranchPoint: BranchPoint = branchMap.keys.head.branchPoint
           val activeBranch: Branch = activeBranchMap(whichBranchPoint.name)
           val(srcSpecX, srcTaskDef) = branchMap(activeBranch)
           val srcSpec = srcSpecX
           val parentReal: Realization = spec2reals.get(origSpec) match {
             case Some(r) => r // config has branch point
             case None => new Realization(v.realization) // config has no branch point
           }
           //mapVal(origSpec, srcSpec, branchMap)
           //System.err.println("Mapping config var with active branch %s to srcSpec %s at srcTask %s with parent real %s".format(activeBranch, srcSpec, srcTaskDef, parentReal))
           new ResolvedSpecType[T](origSpec, srcSpec, srcTaskDef, parentReal)
         }
         case TaskVariable(_,_) => { // not a branch point, but defined elsewhere
           val (srcSpecX, srcTaskDef) = branchMap.values.head
           val srcSpec = srcSpecX
           //System.err.println("Looking for %s in %s".format(origSpec, spec2reals))
           val parentReal = spec2reals(origSpec)
           new ResolvedSpecType[T](origSpec, srcSpec, srcTaskDef, parentReal)
         }
         case _ => { // not a branch point, but either a literal or unbound
           val (srcSpecX, srcTaskDef) = branchMap.values.head
           val srcSpec = srcSpecX
           new ResolvedSpecType[T](origSpec, srcSpec, srcTaskDef, new Realization(v.realization))
         }
       }
     }
     
     // resolve the source spec/task for the selected branch
     def mapVals[T <: Spec](values: Seq[(Spec,Map[Branch,(T,TaskDef)])]): Seq[ResolvedSpecType[T]] = {
       values.map{ case (mySpec, branchMap) => mapVal(mySpec, mySpec, branchMap) }
     }

     val realInputVals: Seq[ResolvedSpec] = mapVals(inputVals)
     val realParamVals: Seq[ResolvedLiteralSpec] = mapVals(paramVals)

     new RealTask(this, realization, realInputVals, realParamVals)
   }
}