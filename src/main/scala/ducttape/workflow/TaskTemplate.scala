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
import grizzled.slf4j.Logging

/**
 * a TaskTemplate is a TaskDef with its input vals, param vals, and branch points resolved
 * 
 * Note that branch point resolution is actually performed here rather than by the metadag.
 * This is because the metadag only encodes temporal dependencies and we must also handle
 * elements such as parameters here, which do not induce temporal dependencies, but may
 * be used in branch points and branch grafts.
 */
class TaskTemplate(val taskDef: TaskDef,
                   val inputVals: Seq[SpecPair],
                   val paramVals: Seq[LiteralSpecPair])
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

   // realize this task by specifying which realizations should be used for this task and parent tasks
   def toRealTask(v: UnpackedWorkVert): RealTask = {
     val realization = new Realization(v.realization)
     debug("Realizing task template %s for realization %s => %s".format(name, v.realization, realization))
     
     // get the specs that are active for this realization
     val specs: Seq[(SpecPair, Realization)] = {
       // v has a list of edges for each parent and a list of the realization for each parent
       // iterate over these in parallel
       // see [[ducttape.hyperdag.UnpackedMetaVertex]]
       v.edges.zip(v.parentRealizations).flatMap { case (hyperedgeElements, parentReals) =>
         // hyperedgeElements is parallel with the parent realizations
         hyperedgeElements.zip(parentReals).flatMap { case (e, parentBranches) =>
           val parentReal = new Realization(parentBranches)
           e.specPairs.map { spec: SpecPair =>
             (spec, parentReal)
           }
         }
       }
     }
     
     // TODO: Assert that all the specs from inputVals made it through the HyperDAG

     val realInputVals: Seq[ResolvedSpec] = for ( (spec, parentReal) <- specs; if spec.isInput) yield {
       debug("Resolved input spec: %s with realization %s".format(spec, parentReal))
       new ResolvedSpecType[Spec](spec.origSpec, spec.srcTask, spec.srcSpec, parentReal)
     }
     val realParamVals: Seq[ResolvedLiteralSpec] = for ( (spec, parentReal) <- specs; if spec.isParam) yield {
       val literalSrcSpec = spec.srcSpec.asInstanceOf[LiteralSpec] // guaranteed to succeed since isParam
       debug("Resolved param spec: %s with realization %s".format(spec, parentReal))
       new ResolvedSpecType[LiteralSpec](spec.origSpec, spec.srcTask, literalSrcSpec, parentReal)
     }

     new RealTask(this, realization, realInputVals, realParamVals)
   }
}
