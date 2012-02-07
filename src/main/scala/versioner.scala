package ducttape.versioner

import collection._

import ducttape.environment.DirectoryArchitect
import ducttape.workflow.Branch

trait WorkflowVersioner {
  def apply(taskName: String, realization: Realization): Int
}

// for use by completion checker
class MostRecentWorkflowVersioner(dirs: DirectoryArchitect) extends WorkflowVersioner {
  import math._

  private var greatestVersionSeen = 0

  def apply(taskName: String, realization: Realization): Int = {
    val dir = dirs.assignUnversionedDir(taskName, realization)
    val mostRecent = dir.listFiles.toList.filter(_.isDirectory).map(_.getName.toInt).max
    greatestVersionSeen = max(greatestVersionSeen, mostRecent)
    mostRecent
  }

  def nextVersion = greatestVersionSeen + 1
}

// for use by all others -- returns the version that should be used
// during the current execution cycle (always the atomic workflow version
// number for new or incomplete tasks, but may be different for previously
// completed tasks/realizations)
class ExecutionVersioner(completedVersions: Map[(String,String), Int],
                         workflowVersion: Int) extends WorkflowVersioner {

  def apply(taskName: String, realization: Realization): Int = {
    completedVersions.get((taskName, realName)) match {
      case Some(prevVer) => prevVer
      case None => workflowVersion
    }
  }
}
