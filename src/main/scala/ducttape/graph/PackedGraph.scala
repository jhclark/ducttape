package ducttape.graph

import java.io.File

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.FileFormatException
import ducttape.syntax.Namespace

import grizzled.slf4j.Logging

class PackedGraph(val wd: WorkflowDefinition) extends Graph {

  val vertices = PackedGraph.recursivelyProcessAST(wd)

}

/**
 * @author Lane Schwartz
 */
object PackedGraph extends Logging {

  private def recursivelyProcessAST(astNode:ASTType) : Seq[Vertex] = {
    val rootVertex = new RootVertex()
    val rest = recursivelyProcessAST(astNode, rootVertex)
    return Seq(rootVertex) ++ rest
  }

  private def recursivelyProcessAST(astNode:ASTType, vertex:Vertex) : Seq[Vertex] = {
    astNode match {
      case abstractSpec            : AbstractSpec[RValue]    => recursivelyProcess( abstractSpec            , vertex)
      case bashCode                : BashCode                => recursivelyProcess( bashCode                , vertex)
      case branchGraft             : BranchGraft             => recursivelyProcess( branchGraft             , vertex)
      case branchGraftElement      : BranchGraftElement      => recursivelyProcess( branchGraftElement      , vertex)
      case branchPointDef          : BranchPointDef          => recursivelyProcess( branchPointDef          , vertex)
      case branchPointRef          : BranchPointRef          => recursivelyProcess( branchPointRef          , vertex)
      case callDefinition          : CallDefinition          => recursivelyProcess( callDefinition          , vertex)
      case comments                : Comments                => recursivelyProcess( comments                , vertex)
      case configAssignment        : ConfigAssignment        => recursivelyProcess( configAssignment        , vertex)
      case configDefinition        : ConfigDefinition        => recursivelyProcess( configDefinition        , vertex)
      case configVariable          : ConfigVariable          => recursivelyProcess( configVariable          , vertex)
      case crossProduct            : CrossProduct            => recursivelyProcess( crossProduct            , vertex)
      case groupDefinition         : GroupDefinition         => recursivelyProcess( groupDefinition         , vertex)
      case literal                 : Literal                 => recursivelyProcess( literal                 , vertex)
      case planDefinition          : PlanDefinition          => recursivelyProcess( planDefinition          , vertex)
      case sequence                : Sequence                => recursivelyProcess( sequence                , vertex)
      case sequentialBranchPoint   : SequentialBranchPoint   => recursivelyProcess( sequentialBranchPoint   , vertex)
      case shorthandBranchGraft    : ShorthandBranchGraft    => recursivelyProcess( shorthandBranchGraft    , vertex)
      case shorthandConfigVariable : ShorthandConfigVariable => recursivelyProcess( shorthandConfigVariable , vertex)
      case shorthandTaskVariable   : ShorthandTaskVariable   => recursivelyProcess( shorthandTaskVariable   , vertex)
      case taskDef                 : TaskDef                 => recursivelyProcess( taskDef                 , vertex)
      case taskHeader              : TaskHeader              => recursivelyProcess( taskHeader              , vertex)
      case taskInputs              : TaskInputs              => recursivelyProcess( taskInputs              , vertex)
      case taskOutputs             : TaskOutputs             => recursivelyProcess( taskOutputs             , vertex)
      case taskPackageNames        : TaskPackageNames        => recursivelyProcess( taskPackageNames        , vertex)
      case taskParams              : TaskParams              => recursivelyProcess( taskParams              , vertex)
      case taskVariable            : TaskVariable            => recursivelyProcess( taskVariable            , vertex)
      case unbound                 : Unbound                 => recursivelyProcess( unbound                 , vertex)
      case workflowDefinition      : WorkflowDefinition      => recursivelyProcess( workflowDefinition      , vertex)
    }
  }


  private def recursivelyProcess( abstractSpec            : AbstractSpec[RValue]     , vertex:Vertex) : Seq[Vertex] = {
    return recursivelyProcessAST(abstractSpec.rval, vertex)
  }

  private def recursivelyProcess( bashCode                : BashCode                 , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( branchGraft             : BranchGraft              , vertex:Vertex) : Seq[Vertex] = {
    return Seq(new BranchGraftVertex(branchGraft))
  }

  private def recursivelyProcess( branchGraftElement      : BranchGraftElement       , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( branchPointDef          : BranchPointDef           , vertex:Vertex) : Seq[Vertex] = {
    val branchPointDefVertex = new BranchPointDefVertex(branchPointDef)
    Edge.connect(branchPointDefVertex, vertex)

    val branchVerticesAndChildren = branchPointDef.specs.flatMap({ spec:Spec =>

      val branchVertex = new BranchVertex(spec)
      Edge.connect(branchVertex, branchPointDefVertex)

      val children = recursivelyProcessAST(spec.rval, branchVertex)

      Seq(branchVertex) ++ children
    })

    return Seq(branchPointDefVertex) ++ branchVerticesAndChildren
  }

  private def recursivelyProcess( branchPointRef          : BranchPointRef           , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( callDefinition          : CallDefinition           , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( comments                : Comments                 , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( configAssignment        : ConfigAssignment         , vertex:Vertex) : Seq[Vertex] = {

    val lhsVertex = new ConfigParamVertex(configAssignment)
    val children  = recursivelyProcessAST(configAssignment.spec.rval, lhsVertex)

    return Seq(lhsVertex) ++ children
  }

  private def recursivelyProcess( configDefinition        : ConfigDefinition         , vertex:Vertex) : Seq[Vertex] = {

    val children = configDefinition.lines.flatMap({ line:ASTType => recursivelyProcessAST(line, vertex)})

    return children
  }

  private def recursivelyProcess( configVariable          : ConfigVariable           , vertex:Vertex) : Seq[Vertex] = {
    return Seq(new ConfigVariableVertex(configVariable))
  }

  private def recursivelyProcess( crossProduct            : CrossProduct             , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( groupDefinition         : GroupDefinition          , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( literal                 : Literal                  , vertex:Vertex) : Seq[Vertex] = {

    val literalVertex = new LiteralVertex(literal)
    Edge.connect(literalVertex, vertex)

    return Seq(literalVertex)
  }

  private def recursivelyProcess( planDefinition          : PlanDefinition           , vertex:Vertex) : Seq[Vertex] = {
    return Seq(new PlanDefinitionVertex(planDefinition))
  }

  private def recursivelyProcess( sequence                : Sequence                 , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( sequentialBranchPoint   : SequentialBranchPoint    , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( shorthandBranchGraft    : ShorthandBranchGraft     , vertex:Vertex) : Seq[Vertex] = {
    val newVertex = new ShorthandBranchGraftVertex(shorthandBranchGraft)
    Edge.connect(newVertex, vertex)
    return Seq(newVertex)
  }

  private def recursivelyProcess( shorthandConfigVariable : ShorthandConfigVariable  , vertex:Vertex) : Seq[Vertex] = {
    val newVertex = new ShorthandConfigVariableVertex(shorthandConfigVariable)
    Edge.connect(newVertex, vertex)
    return Seq(newVertex)
  }

  private def recursivelyProcess( shorthandTaskVariable   : ShorthandTaskVariable    , vertex:Vertex) : Seq[Vertex] = {
    val newVertex = new ShorthandTaskVariableVertex(shorthandTaskVariable)
    Edge.connect(newVertex, vertex)
    return Seq(newVertex)
  }

  private def recursivelyProcess( taskDef                 : TaskDef                  , vertex:Vertex) : Seq[Vertex] = {
    val taskVertex = new TaskVertex(taskDef)

    val children = recursivelyProcessAST(taskDef.header, taskVertex)

    return Seq(taskVertex) ++ children
  }

  private def recursivelyProcess( taskHeader              : TaskHeader               , vertex:Vertex) : Seq[Vertex] = {

    val children = taskHeader.specsList.flatMap({ specs:ASTType => recursivelyProcessAST(specs, vertex)})

    return children
  }

  private def recursivelyProcess( taskInputs              : TaskInputs               , vertex:Vertex) : Seq[Vertex] = {
    val inputVerticesAndChildren = taskInputs.specs.flatMap({ spec:Spec =>

      val inputVertex = new TaskInputVertex(spec, taskInputs.comments.value)
      Edge.connect(inputVertex, vertex)

      val children = recursivelyProcessAST(spec, inputVertex)

      Seq(inputVertex) ++ children
    })

    return inputVerticesAndChildren
  }

  private def recursivelyProcess( taskOutputs             : TaskOutputs              , vertex:Vertex) : Seq[Vertex] = {
    val outputVerticesAndChildren = taskOutputs.specs.flatMap({ spec:Spec =>

      val outputVertex = new TaskOutputVertex(spec, taskOutputs.comments.value)
      Edge.connect(vertex, outputVertex)

      Seq(outputVertex)

// TODO: Consider whether to allow the following:
//      val children = recursivelyProcessAST(spec, outputVertex)
//      Seq(outputVertex) ++ children
    })

    return outputVerticesAndChildren
  }

  private def recursivelyProcess( taskPackageNames        : TaskPackageNames         , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( taskParams              : TaskParams               , vertex:Vertex) : Seq[Vertex] = {
    val paramVerticesAndChildren = taskParams.specs.flatMap({ spec:Spec =>

      val paramVertex = new TaskParamVertex(spec, taskParams.comments.value)
      Edge.connect(paramVertex, vertex)

      val children = recursivelyProcessAST(spec, paramVertex)

      Seq(paramVertex) ++ children
    })

    return paramVerticesAndChildren
  }

  private def recursivelyProcess( taskVariable            : TaskVariable             , vertex:Vertex) : Seq[Vertex] = {
    val newVertex = new TaskVariableVertex(taskVariable)
    Edge.connect(newVertex, vertex)
    return Seq(newVertex)
  }

  private def recursivelyProcess( unbound                 : Unbound                  , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( workflowDefinition      : WorkflowDefinition       , vertex:Vertex) : Seq[Vertex] = {

    vertex match {
      case RootVertex() => /* This space intentionally left blank. */
      case _            => throw new RuntimeException("Workflow definition should have had parent RootVertex, but did not")
    }

    val children = workflowDefinition.elements.flatMap({ element:ASTType => recursivelyProcessAST(element, vertex)})

    return children
  }

}

