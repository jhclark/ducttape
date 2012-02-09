package ducttape.versioner

import collection._

import ducttape.environment.DirectoryArchitect
import ducttape.workflow._
import ducttape.util._

trait WorkflowVersioner {
  def apply(taskName: String, realization: Realization): Int
  def workflowVersion: Int
}

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

// for use by all others -- returns the version that should be used
// during the current execution cycle (always the atomic workflow version
// number for new or incomplete tasks, but may be different for previously
// completed tasks/realizations)
class ExecutionVersioner(completedVersions: Map[(String,Realization), Int],
                         val workflowVersion: Int) extends WorkflowVersioner {

  def apply(taskName: String, realization: Realization): Int = {
    completedVersions.get((taskName, realization)) match {
      case Some(prevVer) => prevVer
      case None => workflowVersion
    }
  }
}
