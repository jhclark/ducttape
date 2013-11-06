package ducttape.workflow.builder

import collection._
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.TaskTemplate
import ducttape.workflow.BranchPoint
import ducttape.hyperdag.PackedVertex

/** Main data structure passed from TaskTemplateBuilder to WorkflowBuilder
 *  storing TaskTemplates so that they can be added to a HyperDAG.
 * 
 * (task, parents) -- Option as None indicates that parent should be a phantom vertex
 * so as not to affect temporal ordering nor appear when walking the DAG
 */
private[builder] class FoundTasks(
  val taskTemplates: Seq[TaskTemplate],
  val parents: Map[TaskTemplate,BranchPointTreeGrafts],
  val branchPoints: Seq[BranchPoint]
)
