package ducttape.viz

import collection._
import ducttape.syntax.AbstractSyntaxTree.TaskDef

object WorkflowViz {
  import ducttape.versioner._
  import ducttape.workflow._
  import ducttape.workflow.Types._

  def toGraphViz(workflow: HyperWorkflow,
                 plannedVertices: Set[(String,Realization)],
                 completed: Set[(String,Realization)] = Set.empty,
                 running: Set[(String,Realization)] = Set.empty,
                 failed: Set[(String,Realization)] = Set.empty) = {

    val str = new StringBuilder(1000)
    str ++= "digraph G {\n"

    def getName(t: Option[TaskDef], r: Realization) = GraphViz.escape("%s/%s".format(t, r.toString))

    // first, list vertices
    for (v: UnpackedWorkVert <- workflow.unpackedWalker(plannedVertices=plannedVertices).iterator) {
      val taskT: TaskTemplate = v.packed.value.get
      val task: RealTask = taskT.realize(v)
      val color = (task.name, task.realization) match {
        case t if completed(t) => "dodgerblue1"
        case t if running(t) => "darkolivegreen4"
        case t if failed(t) => "firebrick"
        case _ => "white"
      }
      str ++= "\"%s\" [fillcolor=%s,style=filled];\n".format(getName(Some(task.taskDef), task.realization), color)
    }

    // now list edges
    for (v: UnpackedWorkVert <- workflow.unpackedWalker(plannedVertices=plannedVertices).iterator) {
      val taskT: TaskTemplate = v.packed.value.get
      val task: RealTask = taskT.realize(v)
      val child = getName(Some(task.taskDef), task.realization)
      task.inputVals.map { inputVal =>
        getName(inputVal.srcTask, inputVal.srcReal)
      }.toSet.foreach { parent: String =>
        if (parent != child)
          str ++= "\"%s\" -> \"%s\";\n".format(parent, child) // TODO: Quote?
      }
    }

    str ++= "}\n"
    str.toString
  }
}
