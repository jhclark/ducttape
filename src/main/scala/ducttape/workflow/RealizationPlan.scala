package ducttape.workflow

import collection._
import ducttape.syntax.AbstractSyntaxTree.PlanDefinition

// TODO: Move filter here, but thoroughly describe the context
// in which it executes

class RealizationPlan(
    val planDef: PlanDefinition,
    val goalTasks: Seq[String],
    val realizations: Map[BranchPoint, Set[String]]) {
  def name: Option[String] = planDef.name
}
