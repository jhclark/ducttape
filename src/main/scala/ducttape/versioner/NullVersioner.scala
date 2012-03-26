package ducttape.versioner

import ducttape.workflow.Realization

// XXX: HACK: Used by workflow vertex filter
object NullVersioner extends WorkflowVersioner {
  override def apply(taskName: String, realization: Realization): Int = -1
  override def workflowVersion = -1
}