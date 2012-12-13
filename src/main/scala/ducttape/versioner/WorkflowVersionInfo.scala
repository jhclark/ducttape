package ducttape.versioner

import ducttape.workflow.RealTaskId

trait WorkflowVersionInfo {
  val version: Int
  def apply(task: RealTaskId): Int
  def get(task: RealTaskId): Option[Int]
}

object WorkflowVersionInfo {
  // hallucinate a new version without actually committing a new version to disk
  def createFake(): WorkflowVersionInfo = {
    throw new Error("Unimplemented")
  }
}
