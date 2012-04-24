package ducttape.workflow

import collection._
import ducttape.exec.UnpackedDagVisitor
import ducttape.workflow.Types.UnpackedWorkVert

object Visitors {
  def visitAll[A <: UnpackedDagVisitor](
      workflow: HyperWorkflow,
      visitor: A,
      plannedVertices: Set[(String,Realization)],
      numCores: Int = 1): A = {
    
    workflow.unpackedWalker(plannedVertices=plannedVertices).foreach(numCores, { v: UnpackedWorkVert => {
      val taskT: TaskTemplate = v.packed.value
      val task: RealTask = taskT.realize(v)
      visitor.visit(task)
    }})
    visitor
  }
}