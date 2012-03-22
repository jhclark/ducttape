package ducttape.exec

import ducttape.Config

import ducttape.versioner.WorkflowVersioner
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.util.Files

object PartialOutputRemover {
  def hasPartialOutput(taskEnv: TaskEnvironment) = taskEnv.where.exists
}

class PartialOutputRemover(conf: Config,
                           dirs: DirectoryArchitect,
                           versions: WorkflowVersioner,
                           partial: Set[(String,Realization)]) extends UnpackedDagVisitor {
  
  override def visit(task: RealTask) {
    val taskEnv = new TaskEnvironment(dirs, versions, task)
    if(partial( (task.name, task.realization) )) {
      Console.err.println("Removing partial output at %s".format(taskEnv.where))
      Files.deleteDir(taskEnv.where)
    }
  }  
}