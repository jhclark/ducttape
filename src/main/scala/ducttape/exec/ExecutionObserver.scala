package ducttape.exec

trait ExecutionObserver {
  def init(exec: Executor) {}
  
  def begin(exec: Executor, taskEnv: FullTaskEnvironment) {}
  def fail(exec: Executor, taskEnv: FullTaskEnvironment) {}
  def succeed(exec: Executor, taskEnv: FullTaskEnvironment) {}

  def skip(exec: Executor, taskEnv: TaskEnvironment) {}
}
