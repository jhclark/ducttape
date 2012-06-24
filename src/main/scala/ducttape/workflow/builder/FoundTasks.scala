package ducttape.workflow.builder

import collection._
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.TaskTemplate
import ducttape.workflow.BranchPoint
import ducttape.workflow.builder.WorkflowBuilder.BranchPointTree
import ducttape.workflow.builder.WorkflowBuilder.BranchPointTreeData
import ducttape.hyperdag.PackedVertex

// (task, parents) -- Option as None indicates that parent should be a phantom vertex
// so as not to affect temporal ordering nor appear when walking the DAG
private[builder] class FoundTasks(
  val taskTemplates: Seq[TaskTemplate],
  val parents: Map[TaskTemplate,BranchPointTreeData],
  val branchPoints: Seq[BranchPoint]
)
