package ducttape.workflow.builder

import ducttape.workflow.Branch
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.SpecTypes.SpecPair
import scala.collection.mutable

/**
 * TODO: Explain to Lane what this class is
 */
private[builder] class TerminalData(
    val task: Option[TaskDef],
    val grafts: Seq[Branch],
    val isParam: Boolean) {

  val specs = new mutable.ArrayBuffer[SpecPair]

      override def toString() = "(%s %s isParam=%s %s)".format(task, grafts, isParam, specs)
}
