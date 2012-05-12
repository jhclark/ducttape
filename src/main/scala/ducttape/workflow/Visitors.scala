package ducttape.workflow

import collection._
import ducttape.exec.UnpackedDagVisitor
import ducttape.workflow.Types.UnpackedWorkVert
import grizzled.slf4j.Logging

object Visitors extends Logging {
  def visitAll[A <: UnpackedDagVisitor](
      workflow: HyperWorkflow,
      visitor: A,
      plannedVertices: Set[(String,Realization)],
      numCores: Int = 1): A = {
    
    info("Visiting workflow")
    workflow.unpackedWalker(plannedVertices=plannedVertices).foreach(numCores, { v: UnpackedWorkVert =>
      val taskT: TaskTemplate = v.packed.value.get
      val task: RealTask = taskT.realize(v)
      info("Visiting %s".format(task))
      visitor.visit(task)
    })
    visitor
  }
}