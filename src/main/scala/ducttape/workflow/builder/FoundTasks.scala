package ducttape.workflow.builder

import collection._
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.TaskTemplate
import ducttape.workflow.BranchInfo
import ducttape.workflow.BranchPoint
import ducttape.hyperdag.PackedVertex

// (task, parents) -- Option as None indicates that parent should be a phantom vertex
// so as not to affect temporal ordering nor appear when walking the DAG
private[builder] class FoundTasks(
  val parents: Map[TaskTemplate,Map[BranchInfo,Set[Option[TaskDef]]]],
  val vertices: Map[String,PackedVertex[TaskTemplate]],
  val branchPoints: Seq[BranchPoint],
  val branchPointsByTask: Map[TaskDef,Set[BranchPoint]]);