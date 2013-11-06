package ducttape.workflow

import collection._
import ducttape.syntax.AbstractSyntaxTree.PlanDefinition

// TODO: Move filter here, but thoroughly describe the context
// in which it executes

/** we may have many realization plans for a single "plan" block -- there will be one for each "reach" clase
 *  the fullName gives information about which clause */
class RealizationPlan(
    val planDef: PlanDefinition,
    val goalTasks: Seq[String],
    val realizations: Map[BranchPoint, Set[String]],
    val fullName: String) {
  def name: Option[String] = planDef.name
}
