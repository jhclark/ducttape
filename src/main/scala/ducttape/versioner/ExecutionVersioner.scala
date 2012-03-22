package ducttape.versioner

import collection._

import ducttape.exec.DirectoryArchitect
import ducttape.workflow.Realization
import ducttape.util.Files

// for use by all others -- returns the version that should be used
// during the current execution cycle (always the atomic workflow version
// number for new or incomplete tasks, but may be different for previously
// completed tasks/realizations)
class ExecutionVersioner(completedVersions: Map[(String,Realization), Int],
                         val workflowVersion: Int) extends WorkflowVersioner {

  override def apply(taskName: String, realization: Realization): Int = {
    completedVersions.get((taskName, realization)) match {
      case Some(prevVer) => prevVer
      case None => workflowVersion
    }
  }
}
