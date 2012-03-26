package ducttape.versioner

import ducttape.workflow.Realization

trait WorkflowVersioner {
  def apply(taskName: String, realization: Realization): Int
  def workflowVersion: Int
}