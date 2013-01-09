package ducttape.versioner

import ducttape.workflow.RealTaskId
import ducttape.workflow.VersionedTaskId

/** concrete implementation is WorkflowVersionStore */
trait WorkflowVersionInfo {
  val version: Int
  def existing: Seq[VersionedTaskId]
  def apply(task: RealTaskId): Int
  def get(task: RealTaskId): Option[Int]
}

object FakeWorkflowVersionInfo extends WorkflowVersionInfo {
  val FAKE_VERSION: Int = 0
  val version: Int = FAKE_VERSION
  def existing: Seq[VersionedTaskId] = Seq.empty
  def apply(task: RealTaskId): Int = FAKE_VERSION
  def get(task: RealTaskId): Option[Int] = Some(FAKE_VERSION)
}

/** fallbackVersion is returned by apply() if there is no existing version for the task
 *  get() will return None if there is no existing version for a task */
class UnionWorkflowVersionInfo(val version: Int,
                               val existing: Seq[VersionedTaskId],
                               val fallbackVersion: Int) extends WorkflowVersionInfo {
  val taskMap: Map[RealTaskId,Int] = {
    existing.map { task: VersionedTaskId => (task.toRealTaskId, task.version) }
  }.toMap
  def apply(task: RealTaskId): Int = taskMap.getOrElse(task, fallbackVersion)
  def get(task: RealTaskId): Option[Int] = taskMap.get(task)
}

object WorkflowVersionInfo {
  // hallucinate a new version without actually committing a new version to disk
  def createFake(): WorkflowVersionInfo = FakeWorkflowVersionInfo
}
