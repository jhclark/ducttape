package ducttape.versioner
import ducttape.util.Files
import java.io.File

class WorkflowVersionHistory(val history: Seq[WorkflowVersionInfo]) {
  lazy val prevVersion: Int = (Seq(0) ++ history.map(_.version)).max
  lazy val nextVersion: Int = prevVersion + 1
}

object WorkflowVersionHistory {
  def load(versionHistoryDir: File) = new WorkflowVersionHistory(
    Files.ls(versionHistoryDir).filter(_.isDirectory).map(dir => WorkflowVersionInfo.load(dir)))
}