package ducttape.workflow

import collection._

// TODO: Move filter here, but thoroughly describe the context
// in which it executes

class RealizationPlan(val name: Option[String], val goalTasks: Seq[String], val realizations: Map[BranchPoint, Set[Branch]]);
