package ducttape.exec

import ducttape.workflow.Realization
import ducttape.workflow.RealTask


trait ExecutionObserver {
  def init(exec: Executor) {}
  
  def begin(exec: Executor, taskEnv: FullTaskEnvironment)
  def fail(exec: Executor, taskEnv: FullTaskEnvironment)
  def succeed(exec: Executor, taskEnv: FullTaskEnvironment)
}