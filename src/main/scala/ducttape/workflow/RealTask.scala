package ducttape.workflow

import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.SpecTypes._

// short for "realized task"
// we might shorten this to Task
class RealTask(val taskT: TaskTemplate,
               val realization: Realization,
               // TODO: Change inputVals and paramVals over to Realization?
               val inputVals: Seq[ResolvedSpec],
               val paramVals: Seq[ResolvedLiteralSpec]) { // workflow version
   def name = taskT.name
   def taskDef = taskT.taskDef
   def comments = taskT.comments
   def packages = taskT.packages
   def inputs = taskT.inputs
   def outputs = taskT.outputs
   def params = taskT.params
   def commands = taskT.commands // TODO: This will no longer be valid once we add in-lines

  // the tasks and realizations that must temporally precede this task (due to having required input files)
   lazy val antecedents: Set[(String, Realization)] = inputVals.collect {
     case inputVal if (inputVal.srcTask.isDefined) => {
       (inputVal.srcTask.get.name, inputVal.srcReal)
     }
   } toSet

  // TODO: Smear hash code better
   override def hashCode() = name.hashCode ^ realization.hashCode
   override def equals(obj: Any) = obj match {
     case that: RealTask => this.name == that.name && this.realization == that.realization
   }

   // TODO: Specialize if we're flat
   override def toString() = "%s/%s".format(name, realization.toString)
 }