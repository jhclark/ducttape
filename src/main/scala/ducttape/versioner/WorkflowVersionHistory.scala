package ducttape.versioner

import ducttape.util.Files
import ducttape.workflow.VersionedTaskId

import collection._
import java.io.File
import grizzled.slf4j.Logging

class WorkflowVersionHistory(val history: Seq[WorkflowVersionInfo]) extends Logging {
  lazy val prevVersion: Option[Int] = history.size match {
    case 0 => None
    case _ => Some(history.map(_.version).max)
  }
  lazy val nextVersion: Int = prevVersion.getOrElse(0) + 1
  def prevVersionInfo: Option[WorkflowVersionInfo] = prevVersion match {
    case None => None
    case Some(i) => Some(history(i))
  }

  /** returns a UnionWorkflowVersionInfo that whose apply method will return the current version
   *  if no existing version exists for a task */
  def union(): WorkflowVersionInfo = {
    val existing: Seq[VersionedTaskId] = history.flatMap { info: WorkflowVersionInfo => info.existing }
    val todo: Seq[VersionedTaskId] = history.flatMap { info: WorkflowVersionInfo => info.todo }
    val all = existing ++ todo
    val curVersion: Int = ( Seq(0) ++ all.map(_.version) ).max

    // TODO: Is this reasonable to combine todo and existing? It's mainly for the benefit
    // of the CompletionChecker, who will use existing to determine what versions already exist
    new UnionWorkflowVersionInfo(curVersion, existing = all, todo = Nil, fallbackVersion=curVersion)
  }
}

object WorkflowVersionHistory extends Logging {
  def load(versionHistoryDir: File) = {
    val errors = new mutable.ArrayBuffer[Throwable]
    val history = new WorkflowVersionHistory(
      Files.ls(versionHistoryDir).filter {
        _.isDirectory
      }.map { dir =>
        try {
          WorkflowVersionStore.load(dir)
        } catch {
          case ex: Throwable => {
            debug(s"Ignoring corrupt or incomplete version: ${dir} --  ${ex.getMessage}")
            errors += ex
            
            // we need to at least have a place-holder for this workflow version number
            // TODO: What if some evil person created a non-integer directory name?
            val versionNumber: Int = dir.getName.toInt
            new CorruptWorkflowVersionInfo(versionNumber)
          }
        }
      }
    )
    if (errors.size > 0) {
      System.err.println(s"WARNING: ${errors} corrupt or incomplete workflow versions found. Ignoring them. (This could be due to upgrading from an older version of ducttape that doesn't support versioning)")
      System.err.println(s"WARNING: Most recent corruption: ${errors.last.getMessage}")
      throw errors.last
    }
    history
  }
}
