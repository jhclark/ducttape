package ducttape.graph

import java.io.File

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.FileFormatException
import ducttape.syntax.Namespace

import grizzled.slf4j.Logging

class PackedGraph(val wd: WorkflowDefinition) extends Graph {

  val vertices = PackedGraph.recursivelyProcessAST(wd)
/*
  PackedGraph.createTaskVertices(wd, this)

  PackedGraph.createTaskInputVertices(this)
  PackedGraph.createTaskParamVertices(this)
  PackedGraph.createTaskOutputVertices(this)

  PackedGraph.processTaskInputVertices(this)
*/
}

/**
 * @author Lane Schwartz
 */
object PackedGraph extends Logging {

//  /**
//   * Creates a map from task names to task-vertex pairs.
//   *
//   * For each task definition in the workflow,
//   * this function creates a unique ID for the task,
//   * and creates a vertex for that task in the provided graph.
//   *
//   * @return A map whose keys are task IDs and whose values are TaskVertex objects
//   */
//  private def createTaskVertices(wd: WorkflowDefinition, graph: Graph) : Unit = {
//    // Start with the list of task definitions from the workflow,
//		wd.tasks.
//		// and iterate over each task definition
//		foreach({ task:TaskDef =>
//
//		  val id : String = task.name.toString()
//      val comment : Option[String] = task.comments.value
//      val vertex = new TaskVertex(id, task, comment)
//
//      // creating a vertex in the graph for each task
//      graph.addVertex(vertex)
//
//		})
//  }
//
//  private def taskSpecVertexID(specName:String, taskName:String) = "$%s@%s".format(specName, taskName)
//
//  /** Creates a vertex in the graph (and associated edge) for each input of each task. */
//  private def createTaskInputVertices(graph: Graph) : Unit = {
//    graph.collect({ case taskVertex:TaskVertex => taskVertex }).foreach({ taskVertex =>
//      taskVertex.contents.header.specsList.collect{ case i:TaskInputs  => i.specs }.flatten.foreach({ input =>
//        val vertexID = taskSpecVertexID(input.name, taskVertex.id)
//        val vertex = new TaskInputVertex(vertexID, input.rval, comment=None)
//        graph.addVertex(vertex)
//        graph.constructEdge(vertex, taskVertex, contents=None, comment=None)
//      })
//    })
//  }
//
//  /** Creates a vertex in the graph (and associated edge) for each parameter of each task. */
//  private def createTaskParamVertices(graph: Graph) : Unit = {
//    graph.collect({ case taskVertex:TaskVertex => taskVertex }).foreach({ taskVertex =>
//      taskVertex.contents.header.specsList.collect{ case i:TaskParams  => i.specs }.flatten.foreach({ param =>
//        val vertexID = taskSpecVertexID(param.name, taskVertex.id)
//        val vertex = new TaskParamVertex(vertexID, param.rval, comment=None)
//        graph.addVertex(vertex)
//        graph.constructEdge(vertex, taskVertex, contents=None, comment=None)
//      })
//    })
//  }
//
//  /** Creates a vertex in the graph (and associated edge) for each output of each task. */
//  private def createTaskOutputVertices(graph: Graph) : Unit = {
//    graph.collect({ case taskVertex:TaskVertex => taskVertex }).foreach({ taskVertex =>
//      taskVertex.contents.header.specsList.collect{ case i:TaskOutputs  => i.specs }.flatten.foreach({ output =>
//        val vertexID = taskSpecVertexID(output.name, taskVertex.id)
//        val vertex = new TaskParamVertex(vertexID, output.rval, comment=None)
//        graph.addVertex(vertex)
//        graph.constructEdge(taskVertex, vertex, contents=None, comment=None)
//      })
//    })
//  }
//
//  /** Creates a vertex in the graph (and associated edge) for each input of each task. */
//  private def processTaskInputVertices(graph: Graph) : Unit = {
//    graph.collect({ case taskInputVertex:TaskInputVertex => taskInputVertex }).foreach({ taskInputVertex =>
//      recursivelyProcessAST(taskInputVertex.contents, taskInputVertex)
////      taskInputVertex.contents.children.foreach { child:ASTType => child match {
////          case rval:RValue => ???
////          case _ => throw new FileFormatException("While processing abstract syntax tree, encountered an unexpected type " + child.toString() + " where an RValue was expected", child)
////        }
////      }
//    })
//  }


  private def recursivelyProcessAST(astNode:ASTType) : Seq[Vertex] = {
    val rootVertex = new RootVertex()
    val rest = recursivelyProcessAST(astNode, rootVertex)
    return Seq(rootVertex) ++ rest
  }

  private def recursivelyProcessAST(astNode:ASTType, vertex:Vertex) : Seq[Vertex] = {
    astNode match {
      case Unbound() => ???
      case Literal(value:String) => recursivelyProcessLiteral(vertex, value)
      case ConfigVariable(value:String) => ???
      case TaskVariable(taskName: String, value: String) => recursivelyProcessTaskVariable(vertex, taskName, value)
      case ShorthandTaskVariable(taskName: String) => ???
      case ShorthandConfigVariable() => ???
      case Sequence(start: BigDecimal,
                      end: BigDecimal,
                      increment: BigDecimal) => ???
      case SequentialBranchPoint(branchPointName: Option[String],
                                 sequence: Sequence) => ???
      case BranchGraftElement(branchPointName: String, branchName: String) => ???
      case BranchGraft(variableName: String,
                         taskName: Option[String],
                         branchGraftElements: Seq[BranchGraftElement]) => recursivelyProcessBranchGraft(vertex, variableName, taskName, branchGraftElements)
      case ShorthandBranchGraft(taskName: String,
                         branchGraftElements: Seq[BranchGraftElement]) => ???
      case AbstractSpec(name: String, rval:RValue, dotVariable: Boolean) => ???
      case ConfigAssignment(spec: Spec, comments: Comments) => recursivelyProcessConfigAssignment(vertex, spec, comments)
      case TaskInputs(specs: Seq[Spec], comments: Comments) => ???
      case TaskParams(specs: Seq[Spec], comments: Comments) => recursivelyProcessTaskParams(vertex, specs, comments)
      case TaskOutputs(specs: Seq[Spec], comments: Comments) => ???
      case TaskPackageNames(specs: Seq[Spec], comments: Comments) => ???
      case BranchPointDef(name: Option[String], specs: Seq[Spec]) => recursivelyProcessBranchPointDef(vertex, name, specs)
      case BranchPointRef(name: String, branchNames: Seq[ASTType]) => ???
      case BashCode(code: String, vars: Set[String]) => ???
      case TaskHeader(specsList: List[Specs]) => recursivelyProcessTaskHeader(vertex, specsList)
      case TaskDef(comments: Comments,
                keyword: String,
                name: Namespace,
                header: TaskHeader,
                commands: BashCode) => recursivelyProcessTaskDef(vertex, keyword, comments, name, header, commands)
      case CallDefinition(comments: Comments,
                       name: String,
                       header: TaskHeader,
                       functionName: Namespace) => ???
      case GroupDefinition(comments: Comments,
                        keyword: String,
                        name: Namespace,
                        header: TaskHeader,
                        blocks: Seq[Block]) => ???
      case ConfigDefinition(keyword: String,
                         comments: Comments,
                         name: Option[String],
                         lines: Seq[ConfigAssignment]) => recursivelyProcessConfigDefinition(vertex, keyword, comments, name, lines)
      case CrossProduct(goals: Seq[String], value: Seq[BranchPointRef]) => ???
      case PlanDefinition(comments: Comments,
                       name: Option[String],
                       crossProducts: Seq[CrossProduct]) => ???
      case Comments(value:Option[String]) => ???
      case WorkflowDefinition(elements: Seq[ASTType], _, _, _) => recursivelyProcessWorkflowDefinition(vertex, elements)
    }
  }

  private def recursivelyProcessTaskParams(vertex:Vertex, specs:Seq[Spec], comments:Comments) : Seq[Vertex] = {

    val paramVerticesAndChildren = specs.flatMap({ spec:Spec =>

      val paramVertex = new TaskParamVertex(id=spec.name, contents=spec.rval, comment=comments.value)
      Edge.connect(paramVertex, vertex)

      val children = recursivelyProcessAST(spec, paramVertex)

      Seq(paramVertex) ++ children
    })

    return paramVerticesAndChildren
  }

  private def recursivelyProcessTaskHeader(vertex:Vertex, specsList: List[Specs]) : Seq[Vertex] = {

    def children = specsList.flatMap({ specs:ASTType => recursivelyProcessAST(specs, vertex)})

    return children
  }

  private def recursivelyProcessTaskDef(vertex:Vertex, keyword:String, comments:Comments, name:Namespace, header:TaskHeader, commands:BashCode) : Seq[Vertex] = {

    val taskVertex = new TaskVertex(id=name.toString(), contents=commands, comment=comments.value)

    val children = recursivelyProcessAST(header, taskVertex)

    return Seq(taskVertex) ++ children
  }

  private def recursivelyProcessLiteral(vertex:Vertex, value:String) : Seq[Vertex] = {

    val literalVertex = new LiteralVertex(value, comment=None)
    Edge.connect(literalVertex, vertex)

    return Seq(literalVertex)
  }

  private def recursivelyProcessConfigAssignment(
      vertex:Vertex,
      spec: Spec, comments: Comments) : Seq[Vertex] = {

    val configVertex = vertex match {
      case v:ConfigVertex => v
      case _              => throw new RuntimeException("Config assignment should have had parent ConfigVertex, but did not")
    }

    val lhsVertex = new ConfigParamVertex(spec.name, spec.rval, comments.value)
    Edge.connect(lhsVertex, configVertex)

    val children = recursivelyProcessAST(spec.rval, lhsVertex)

    return Seq(lhsVertex) ++ children
  }

  private def recursivelyProcessConfigDefinition(
      vertex:Vertex,
      keyword: String,
      comments: Comments,
      name: Option[String],
      lines: Seq[ConfigAssignment]) : Seq[Vertex] = {

      val id = name match {
        case Some(s) => keyword + " " + s
        case None    => keyword
      }

      val configVertex = new ConfigVertex(id, keyword, comments.value, name)

      val children = lines.flatMap({ line:ASTType => recursivelyProcessAST(line, configVertex)})
      //children.foreach({child => Edge.connect(child, configVertex) })

      return Seq(configVertex) ++ children
  }

  private def recursivelyProcessWorkflowDefinition(vertex:Vertex, elements: Seq[ASTType]) : Seq[Vertex] = {

    vertex match {
      case RootVertex() => /* This space intentionally left blank. */
      case _            => throw new RuntimeException("Workflow definition should have had parent RootVertex, but did not")
    }

    val children = elements.flatMap({ element:ASTType => recursivelyProcessAST(element, vertex)})

    return children

  }


  private def recursivelyProcessBranchPointDef(parent:Vertex, name: Option[String], specs: Seq[Spec]) : Seq[Vertex] = {
	  return Seq()
  }

  private def recursivelyProcessTaskVariable(parent:Vertex, taskName: String, value: String) : Seq[Vertex] = {
	  return Seq()
  }

  private def recursivelyProcessBranchGraft(parent:Vertex,
		  variableName: String,
		  taskName: Option[String],
		  branchGraftElements: Seq[BranchGraftElement]) : Seq[Vertex] = {

      return Seq()

  }
}
