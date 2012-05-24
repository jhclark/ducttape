package ducttape.versioner

import ducttape.util.Files
import java.io.File

class WorkflowVersionHistory(val history: Seq[WorkflowVersionInfo]) {
  lazy val prevVersion: Int = (Seq(0) ++ history.map(_.version)).max
  lazy val nextVersion: Int = prevVersion + 1
}

object WorkflowVersionHistory {
  def load(versionHistoryDir: File) = new WorkflowVersionHistory(
    Files.ls(versionHistoryDir).filter {
      _.isDirectory
    }.map { dir =>
      try {
        Some(WorkflowVersionInfo.load(dir))
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