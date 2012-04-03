package ducttape.workflow

import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.TaskDef

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
   def packages = taskT.packages
   def inputs = taskT.inputs
   def outputs = taskT.outputs
   def params = taskT.params
   def commands = taskT.commands // TODO: This will no longer be valid once we add in-lines

  // the tasks and realizations that must temporally precede this task (due to having required input files)
   lazy val antecedents: Set[(String, Realization)] = {
     for( (inSpec, srcSpec, srcTaskDef, srcRealization) <- inputVals) yield {
       (srcTaskDef.name, new Realization(srcRealization)) // TODO: change seq[branch] to realization?
     }
   }.filter{case (srcTaskDefName, _) => srcTaskDefName != taskT.name }.toSet

  // TODO: Smear hash code better
   override def hashCode() = name.hashCode ^ realization.hashCode ^ version
   override def equals(obj: Any) = obj match {
     case that: RealTask => this.name == that.name && this.realization == that.realization && this.version == that.version
   }

   override def toString() = "%s/%s".format(name, realization.toString)
 }