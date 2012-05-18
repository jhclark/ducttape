package ducttape.exec

import collection._
import ducttape.workflow.Realization
import ducttape.viz.WorkflowViz
import ducttape.util.Files
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.RealTask

class GraphVizExecutionObserver extends ExecutionObserver {
  
  // TODO: Construct set elsewhere?
  val completed = new mutable.HashSet[(String,Realization)]
  val running = new mutable.HashSet[(String,Realization)]
  val failed = new mutable.HashSet[(String,Realization)]
  
  override def init(exec: Executor) {
    completed ++= exec.alreadyDone
    exec.dirs.xdotFile.synchronized {
      val viz = WorkflowViz.toGraphViz(exec.workflow, exec.plannedVertices, completed, running, failed)
      Files.write(viz, exec.dirs.xdotFile)
    }
  }
  
  override def begin(exec: Executor, task: RealTask) {
    running += ((task.name, task.realization))
    exec.dirs.xdotFile.synchronized {
      val viz = WorkflowViz.toGraphViz(exec.workflow, exec.plannedVertices, completed, running, failed)
      Files.write(viz, exec.dirs.xdotFile)
    }
  }
  
  override def fail(exec: Executor, task: RealTask) {
    failed += ((task.name, task.realization))
    running -= ((task.name, task.realization))
    exec.dirs.xdotFile.synchronized {
      val viz = WorkflowViz.toGraphViz(exec.workflow, exec.plannedVertices, completed, running, failed)
      Files.write(viz, exec.dirs.xdotFile)
    }
  }
  
  override def complete(exec: Executor, task: RealTask) {
    completed += ((task.name, task.realization))
    running -= ((task.name, task.realization))
    exec.dirs.xdotFile.synchronized {
      val viz = WorkflowViz.toGraphViz(exec.workflow, exec.plannedVertices, completed, running, failed)
      Files.write(viz, exec.dirs.xdotFile)
    }
  }
}