package ducttape.exec

import ducttape.workflow.Realization
import ducttape.workflow.RealTask

trait ExecutionObserver {
  def init(exec: Executor);
  def begin(exec: Executor, task: RealTask)
  def fail(exec: Executor, task: RealTask)
  def complete(exec: Executor, task: RealTask)
}