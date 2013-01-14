package ducttape.versioner

import ducttape.util.Files
import ducttape.workflow.VersionedTaskId
import java.io.File

class WorkflowVersionHistory(val history: Seq[WorkflowVersionInfo]) {
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
    val curVersion: Int = ( Seq(0) ++ existing.map(_.version) ).max
    new UnionWorkflowVersionInfo(curVersion, existing, fallbackVersion=curVersion)
  }
}

object WorkflowVersionHistory {
  def load(versionHistoryDir: File) = new WorkflowVersionHistory(
    Files.ls(versionHistoryDir).filter {
      _.isDirectory
    }.map { dir =>
      try {
        Some(WorkflowVersionStore.load(dir))
      } catch {
        case ex => {
          System.err.println(s"WARNING: Ignoring corrupt or incomplete version: ${dir} --  ${ex.getMessage}")
          System.err.println("NOTE: This warning could be due to upgrading from an older version of ducttape that doesn't support versioning")
          None
        }
      }
    }.collect {
      // only keep versions that are non-broken
      case Some(info) => info
    }
  )
}
