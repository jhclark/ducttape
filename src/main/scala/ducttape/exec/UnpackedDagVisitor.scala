package ducttape.exec

import ducttape.workflow.VersionedTask

trait UnpackedDagVisitor {
  def visit(task: VersionedTask)
}
