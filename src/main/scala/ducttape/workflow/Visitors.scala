package ducttape.workflow

import collection._
import ducttape.exec.UnpackedDagVisitor
import ducttape.versioner.WorkflowVersionInfo
import ducttape.workflow.Types.UnpackedWorkVert
import grizzled.slf4j.Logging

object Visitors extends Logging {
  def visitAll[A <: UnpackedDagVisitor](
      workflow: HyperWorkflow,
      visitor: A,
      planPolicy: PlanPolicy,
      workflowVersion: WorkflowVersionInfo,
      numCores: Int = 1): A = {
    
    debug("Visiting workflow")
    workflow.unpackedWalker(planPolicy).foreach(numCores, { v: UnpackedWorkVert =>
      val taskT: TaskTemplate = v.packed.value.get
      val task: VersionedTask = taskT.toRealTask(v).toVersionedTask(workflowVersion)
      debug("Visiting %s".format(task))
      visitor.visit(task)
    })
    visitor
  }
}
