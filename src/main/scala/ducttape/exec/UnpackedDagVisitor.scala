package ducttape.exec

import ducttape.workflow.RealTask
import ducttape.workflow.VersionedTask

// TODO: Move this to its own file
trait UnpackedRealDagVisitor {
  def visit(task: RealTask)
}

// TODO: Add Versioned to name
trait UnpackedDagVisitor {
  def visit(task: VersionedTask)
}
