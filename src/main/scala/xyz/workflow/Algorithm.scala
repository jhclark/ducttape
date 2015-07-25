package xyz.workflow

import ducttape.graph.Edge
import ducttape.graph.Graph
import ducttape.graph.Vertex
import ducttape.graph.TaskVertex
import ducttape.graph.TaskInputVertex
import ducttape.graph.TaskOutputVertex
import ducttape.graph.TaskParamVertex

import ducttape.syntax.BashCode
import ducttape.syntax.AbstractSyntaxTree.Block
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.Specs
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.TaskInputs
import ducttape.syntax.AbstractSyntaxTree.TaskOutputs
import ducttape.syntax.AbstractSyntaxTree.TaskPackageNames
import ducttape.syntax.AbstractSyntaxTree.TaskParams
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition


class PackedGraph(val wd: WorkflowDefinition) extends Graph {

	PackedGraph.createTaskVertices(wd, this)
  PackedGraph.createTaskInputVertices(this)
  PackedGraph.createTaskParamVertices(this)
  PackedGraph.createTaskOutputVertices(this)

}

/**
 * @author Lane Schwartz
 */
object PackedGraph {

  /**
   * Creates a map from task names to task-vertex pairs.
   *
   * For each task definition in the workflow,
   * this function creates a unique ID for the task,
   * and creates a vertex for that task in the provided graph.
   *
   * @return A map whose keys are task IDs and whose values are TaskVertex objects
   */
  private def createTaskVertices(wd: WorkflowDefinition, graph: Graph) : Unit = {
    // Start with the list of task definitions from the workflow,
		(wd.tasks ++ wd.functionCallTasks).
		// and iterate over each task definition
		foreach({ task:TaskDef =>

		  val id : String = task.name.toString()
      val comment : Option[String] = task.comments.value
      val vertex = new TaskVertex(id, task, comment)

      // creating a vertex in the graph for each task
      graph.addVertex(vertex)

		})
  }

  private def taskSpecVertexID(specName:String, taskName:String) = "$%s@%s".format(specName, taskName)

  /** Creates a vertex in the graph (and associated edge) for each input of each task. */
  private def createTaskInputVertices(graph: Graph) : Unit = {
    graph.collect({ case taskVertex:TaskVertex => taskVertex }).foreach({ taskVertex =>
      taskVertex.contents.header.specsList.collect{ case i:TaskInputs  => i.specs }.flatten.foreach({ input =>
        val vertexID = taskSpecVertexID(input.name, taskVertex.id)
        val vertex = new TaskInputVertex(vertexID, input.rval, comment=None)
        graph.constructEdge(vertex, taskVertex, contents=None, comment=None)
      })
    })
  }

  /** Creates a vertex in the graph (and associated edge) for each parameter of each task. */
  private def createTaskParamVertices(graph: Graph) : Unit = {
    graph.collect({ case taskVertex:TaskVertex => taskVertex }).foreach({ taskVertex =>
      taskVertex.contents.header.specsList.collect{ case i:TaskParams  => i.specs }.flatten.foreach({ param =>
        val vertexID = taskSpecVertexID(param.name, taskVertex.id)
        val vertex = new TaskParamVertex(vertexID, param.rval, comment=None)
        graph.constructEdge(vertex, taskVertex, contents=None, comment=None)
      })
    })
  }

  /** Creates a vertex in the graph (and associated edge) for each output of each task. */
  private def createTaskOutputVertices(graph: Graph) : Unit = {
    graph.collect({ case taskVertex:TaskVertex => taskVertex }).foreach({ taskVertex =>
      taskVertex.contents.header.specsList.collect{ case i:TaskOutputs  => i.specs }.flatten.foreach({ output =>
        val vertexID = taskSpecVertexID(output.name, taskVertex.id)
        val vertex = new TaskParamVertex(vertexID, output.rval, comment=None)
        graph.constructEdge(taskVertex, vertex, contents=None, comment=None)
      })
    })
  }

}
