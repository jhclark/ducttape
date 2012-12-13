package ducttape.versioner

import ducttape.util.Files
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

  def union(): WorkflowVersionInfo = {
    // TODO: Produce something info-like that returns a version from any previous version
    throw new Error("Unimplemented")
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
          System.err.println("Version is corrupt or incomplete, DELETING: %s: %s".format(dir, ex.getMessage))
          val DELAY_SECS = 3
          Thread.sleep(DELAY_SECS)
          Files.deleteDir(dir)
          None
        }
      }
    }.collect {
      // only keep versions that are non-broken
      case Some(info) => info
    }
  )
}
