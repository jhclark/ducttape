package ducttape.exec

import collection._

import ducttape.Config
import ducttape.versioner.WorkflowVersioner
import ducttape.workflow.Realization
import ducttape.workflow.RealTask

// dirs and versions are unimportant other than being required to generate the TaskEnvironment
class PackageFinder(conf: Config,
                    dirs: DirectoryArchitect,
                    versions: WorkflowVersioner,
                    todo: Set[(String,Realization)]) extends UnpackedDagVisitor {
  
  val packages = new mutable.HashSet[String]
  
  override def visit(task: RealTask) {
    if(todo((task.name, task.realization))) {
      val taskEnv = new TaskEnvironment(dirs, versions, task)
      packages ++= taskEnv.packageNames
    }
  }
}