// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

trait ExecutionObserver {
  def init(exec: Executor) {}
  
  def begin(exec: Executor, taskEnv: FullTaskEnvironment) {}
  def fail(exec: Executor, taskEnv: FullTaskEnvironment) {}
  def succeed(exec: Executor, taskEnv: FullTaskEnvironment) {}

  def skip(exec: Executor, taskEnv: TaskEnvironment) {}
}
