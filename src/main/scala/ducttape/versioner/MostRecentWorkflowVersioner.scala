package ducttape.versioner

import ducttape.exec.DirectoryArchitect
import ducttape.workflow.Realization
import ducttape.util.Files

// for use by completion checker
class MostRecentWorkflowVersioner(dirs: DirectoryArchitect) extends WorkflowVersioner {
  import math._

  private var greatestVersionSeen = 0

  override def apply(taskName: String, realization: Realization): Int = {
    val dir = dirs.assignUnversionedDir(taskName, realization)
    val mostRecent = {
      val candidates: Seq[Int] = Files.ls(dir).filter(_.isDirectory).map(_.getName.toInt)
      candidates match {
        case Seq() => 0 // this task has never been run before
        case _ => candidates.max // use the most recent
      }
    }
    greatestVersionSeen = max(greatestVersionSeen, mostRecent)
    mostRecent
  }

  def nextVersion = greatestVersionSeen + 1
  override def workflowVersion = nextVersion
}