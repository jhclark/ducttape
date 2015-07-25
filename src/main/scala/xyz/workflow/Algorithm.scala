package xyz.workflow

import ducttape.graph.Edge
import ducttape.graph.Graph
import ducttape.graph.Vertex
import ducttape.graph.VertexType

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

/**
 * @author Lane Schwartz
 */
object Algorithm {

  private class TaskVertex(val task:TaskDef, val vertex:Vertex)

  /**
   * Creates a map from task names to task-vertex pairs.
   *
   * For each task definition in the workflow,
   * this function creates a unique ID for the task,
   * and creates a vertex for that task in the provided graph.
   *
   * @return A map whose keys are task IDs and whose values are TaskVertex objects
   */
  private def createTaskMap(wd: WorkflowDefinition, graph: Graph) : Map[String, TaskVertex] = {
      // Start with the list of task definitions from the workflow,
      (wd.tasks ++ wd.functionCallTasks).
      // call the map method, passing in an anonymous function capable of creating a tuple given a task definition.
      map({ task:TaskDef =>

      // Create a vertex in the graph for each task
      val id : String = task.name.toString()
      val comment : Option[String] = task.comments.value
      val vertexType : VertexType = VertexType.Task
      val contents = task.commands
      val vertex = graph.addVertex(id, vertexType, contents, comment)

      // Construct a tuple consisting of the id and a task-vertex object
      (id, new TaskVertex(task, vertex))

    })
    // The result of the above map method is a sequence of tuples,
    // which we now convert to a Map object using the toMap function
    .toMap
  }

  /** Creates a vertex in the graph (and associated edge) for each input, output, and parameter of each task. */
  private def processTask(taskMap : Map[String, TaskVertex], graph: Graph) : Unit = {
//    graph.foreach({ vertex =>
//
//      val id = vertex.id

    taskMap.foreach({ case (key,value) =>

      val taskVertex = value.vertex
      val task = value.task
      val id = key

      def constructVertex(spec:Spec, vertexType: VertexType, contents:Any) : Vertex = {
        val vertexID = "$%s@%s".format(spec.name, id)
        val vertex = graph.addVertex(vertexID, vertexType, contents, comment=None)
        return vertex
      }

      val inputs  = task.header.specsList.collect{ case i:TaskInputs  => i.specs }.flatten
      val params  = task.header.specsList.collect{ case p:TaskParams  => p.specs }.flatten
      val outputs = task.header.specsList.collect{ case o:TaskOutputs => o.specs }.flatten

      inputs.foreach({ input => graph.constructEdge(constructVertex(input, VertexType.TaskInput, input.rval), taskVertex, contents=None, comment=None) })
      params.foreach({ param => graph.constructEdge(constructVertex(param, VertexType.TaskParam, param.rval), taskVertex, contents=None, comment=None) })

      outputs.foreach({ output => graph.constructEdge(taskVertex, constructVertex(output, VertexType.TaskOutput, output.rval), contents=None, comment=None) })

    })
  }


  def foo(wd: WorkflowDefinition) {

    val graph = new Graph()

    val taskMap : Map[String, TaskVertex] = createTaskMap(wd, graph)

    processTask(taskMap, graph)



    System.out.println(graph.toString())
  }


}
