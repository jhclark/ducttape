package ducttape.exec

import ducttape.workflow.RealTask

trait UnpackedDagVisitor {
  def visit(task: RealTask)
}