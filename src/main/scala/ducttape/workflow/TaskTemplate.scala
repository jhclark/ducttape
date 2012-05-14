package ducttape.workflow

import collection._
import ducttape.workflow.SpecTypes._
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.ConfigVariable
import ducttape.hyperdag.HyperEdge
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.SequentialBranchPoint
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.syntax.AbstractSyntaxTree.BranchGraft
import ducttape.syntax.AbstractSyntaxTree.Literal
import ducttape.syntax.AbstractSyntaxTree.Unbound
import ducttape.syntax.FileFormatException
import ducttape.syntax.AbstractSyntaxTree.TaskVariable
import ducttape.util.BranchPrefixTreeMap
import grizzled.slf4j.Logging

/**
 * a TaskTemplate is a TaskDef with its input vals, param vals, and branch points resolved
 * 
 * Note that branch point resolution is actually performed here rather than by the metadag.
 * This is because the metadag only encodes temporal dependencies and we must also handle
 * elements such as parameters here, which do not induce temporal dependencies, but may
 * be used in branch points and branch grafts.
 */
// TODO: fix these insane types for inputVals and paramVals
class TaskTemplate(val taskDef: TaskDef,
                   val inputVals: Seq[ResolvableSpecType[Spec]],
                   val paramVals: Seq[ResolvableSpecType[LiteralSpec]])
  extends Logging {
   def name = taskDef.name
   def comments = taskDef.comments
   def packages = taskDef.packages
   def inputs = taskDef.inputs
   def outputs = taskDef.outputs
   def params = taskDef.params
   def commands = taskDef.commands

   override def hashCode() = name.hashCode
   override def equals(other: Any) = other match { case that: TaskTemplate => this.name == that.name }
   override def toString() = name
   
   // NOTE: MEMORY WARNING: These realizations are not uniqued in any way. We might want to pool them at some point!

   // realize this task by specifying one branch per branch point
   // activeBranches should contain only the hyperedges encountered up until this vertex
   // with the key being the branchPointNames
   def realize(v: UnpackedWorkVert): RealTask = {
     // TODO: Assert all of our branch points are satisfied
     // TODO: We could try this as a view.map() instead of just map() to only calculate these on demand...
     
     val realization = new Realization(v.realization)
     
     debug("Realizing task template %s for realization %s".format(name, v.realization))

     // TODO: do a bit of sanity checking, but now with prefix tree map
//     for (branchPoint <- branchPoints) {
//       assert(activeBranchMap.contains(branchPoint.name),
//              "Required branch point for this task '%s' not found in active branch points '%s'"
//              .format(branchPoint.name, activeBranchMap.keys.mkString("-")))
//     }

     // iterate over the hyperedges selected in this realization
     // remember: *every* metaedge has exactly one active incoming hyperedge
     // this annotation on the plain edges is created in WorkflowBuilder.build()
     // plain edges are associated with the *original* specs (e.g. branch points)
     // not with the final resolved specs!
     val spec2reals = new mutable.HashMap[Spec, Realization]
     debug("TaskTemplate %s: Have %d incoming active hyperedges".format(name, v.edges.size))
     for ( (edgeBundleX, parentRealsByE: Seq[Seq[Branch]]) <- v.edges.zip(v.parentRealizations)) {
       val edgeBundle: Seq[Seq[Spec]] = edgeBundleX
       // TODO: Why would we ever have a null here?
       val edges = edgeBundle.zip(parentRealsByE).filter { case (e, eReals) => e != null }
       debug("TaskTemplate %s: Hyperedge %s has %d plain edges".format(name, edgeBundle, edges.size))
       for ( (specsX, srcRealX) <- edges) {
         val specs: Seq[Spec] = specsX
         val srcReal: Seq[Branch] = srcRealX
         debug("TaskTemplate %s: Edge has %d specs".format(name, specs.size))
         for (spec <- specs) {
           debug("TaskTemplate %s: Spec %s has source real: %s".format(name, spec, srcReal))
           spec2reals += spec -> new Realization(srcReal) // TODO: Pool realizations?
         }
       }
     }
     
     // resolve the source spec/task for the selected branch
     // and return the 
     def resolveVal[SpecT <: Spec](
         origSpec: Spec,
         curSpec: Spec,
         branchMap: BranchPrefixTreeMap[(SpecT,Option[TaskDef])]
         ): ResolvedSpecType[SpecT] = {
       
       def handleBranchPoint(branchPointNameOpt: Option[String]) = {
         val branchPointName = branchPointNameOpt match {
           case Some(name) => name
           case None => throw new RuntimeException("Branch point name is required (this should have already been checked)")
         }
         val (srcSpecX, srcTaskDef) = branchMap(realization)
         val srcSpec: SpecT = srcSpecX
         val parentReal = spec2reals(origSpec)
         new ResolvedSpecType[SpecT](origSpec, srcSpec, srcTaskDef, parentReal)
       }
       
       // TODO: Can we replace all of this with a single lookup in the branchMap?
       curSpec.rval match {
         case BranchPointDef(branchPointNameOpt, _) => handleBranchPoint(branchPointNameOpt)
         case SequentialBranchPoint(branchPointNameOpt, _) => handleBranchPoint(branchPointNameOpt)
         case ConfigVariable(_) => {
           // config variables can, in turn, define branch points, so we must be careful
           // TODO: Isn't this resolved during workflow building now?
           val (srcSpecX, srcTaskDef) = branchMap(realization)
           val srcSpec = srcSpecX
           val parentReal: Realization = spec2reals.get(origSpec) match {
             case Some(r) => r // config has branch point
             case None => new Realization(v.realization) // config has no branch point
           }
           //mapVal(origSpec, srcSpec, branchMap)
           //System.err.println("Mapping config var with active branch %s to srcSpec %s at srcTask %s with parent real %s".format(activeBranch, srcSpec, srcTaskDef, parentReal))
           new ResolvedSpecType[SpecT](origSpec, srcSpec, srcTaskDef, parentReal)
         }
         case TaskVariable(_,_) | BranchGraft(_,_,_) => { // not a branch point, but defined elsewhere
           val (srcSpecX, srcTaskDef) = branchMap.only
           val srcSpec = srcSpecX
           val parentReal = spec2reals(origSpec)
           //System.err.println("Looking for %s in %s: %s".format(origSpec, spec2reals, parentReal))
           new ResolvedSpecType[SpecT](origSpec, srcSpec, srcTaskDef, parentReal)
         }
         case Literal(_) | Unbound() => { // not a branch point, but either a literal or unbound
           val (srcSpecX, srcTaskDef) = branchMap.only
           val srcSpec = srcSpecX
           new ResolvedSpecType[SpecT](origSpec, srcSpec, srcTaskDef, new Realization(v.realization))
         }
       }
     }
     
     // resolve the source spec/task for the selected branch
     def resolveVals[T <: Spec](values: Seq[ResolvableSpecType[T]]): Seq[ResolvedSpecType[T]] = {
       values.map { v => resolveVal(v.mySpec, v.mySpec, v.branchMap) }
     }
     
     v.edges.zip(v.parentRealizations).map { case (hyperedgeElements, parentReals) =>
       hyperedgeElements.zip(parentReals).map { case (e, parentReal) =>
         val specsForParent: Seq[Spec] = e
       }
     }

     val realInputVals: Seq[ResolvedSpec] = resolveVals(inputVals)
     val realParamVals: Seq[ResolvedLiteralSpec] = resolveVals(paramVals)

     new RealTask(this, realization, realInputVals, realParamVals)
   }
}