package ducttape.exec

import ducttape.workflow.TaskTemplate

// TODO: Abstract beyond just workflows?
trait PackedDagVisitor {
  def visit(task: TaskTemplate)
}