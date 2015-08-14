package ducttape.graph

import ducttape.syntax.AbstractSyntaxTree._

class GraphFragments(astNode:WorkflowDefinition) {

  val vertices:Seq[Vertex] = {
    val rootVertex = new RootVertex()
    val rest = GraphFragments.recursivelyProcessAST(astNode, rootVertex)
    Seq(rootVertex) ++ rest
  }

}


/**
 * Collection of recursive utility functions to transform an abstract syntax tree into a partially connected graph.
 *
 * @author Lane Schwartz
 */
object GraphFragments {

  private def recursivelyProcessAST(astNode:ASTType, vertex:Vertex) : Seq[Vertex] = {
    astNode match {
      case actionDef               : ActionDef               => recursivelyProcess( actionDef               , vertex)
      case baselineBlockDef        : BaselineBlockDef        => recursivelyProcess( baselineBlockDef        , vertex)
      case branchBlockDef          : BranchBlockDef          => recursivelyProcess( branchBlockDef          , vertex)
      case bashCode                : BashCode                => recursivelyProcess( bashCode                , vertex)
      case branchGraft             : BranchGraft             => recursivelyProcess( branchGraft             , vertex)
      case branchGraftElement      : BranchGraftElement      => recursivelyProcess( branchGraftElement      , vertex)
      case branchPointBlock        : BranchPointBlock        => recursivelyProcess( branchPointBlock        , vertex)
      case branchPointDef          : BranchPointDef          => recursivelyProcess( branchPointDef          , vertex)
      case branchPointRef          : BranchPointRef          => recursivelyProcess( branchPointRef          , vertex)
      case branchSpec              : BranchSpec[RValue]      => recursivelyProcess( branchSpec              , vertex)
      case callDefinition          : CallDefinition          => recursivelyProcess( callDefinition          , vertex)
      case comments                : Comments                => recursivelyProcess( comments                , vertex)
      case configAssignment        : ConfigAssignment        => recursivelyProcess( configAssignment        , vertex)
      case configDefinition        : ConfigDefinition        => recursivelyProcess( configDefinition        , vertex)
      case configParamSpec         : ConfigParamSpec[RValue] => recursivelyProcess( configParamSpec         , vertex)
      case configVariable          : ConfigVariable          => recursivelyProcess( configVariable          , vertex)
      case crossProduct            : CrossProduct            => recursivelyProcess( crossProduct            , vertex)
      case funcDef                 : FuncDef                 => recursivelyProcess( funcDef                 , vertex)
      case groupDefinition         : GroupDef                => recursivelyProcess( groupDefinition         , vertex)
      case literal                 : Literal                 => recursivelyProcess( literal                 , vertex)
      case packageDef              : PackageDef              => recursivelyProcess( packageDef              , vertex)
      case packageSpec             : PackageSpec             => recursivelyProcess( packageSpec             , vertex)
      case planDefinition          : PlanDefinition          => recursivelyProcess( planDefinition          , vertex)
      case sequence                : Sequence                => recursivelyProcess( sequence                , vertex)
      case sequentialBranchPoint   : SequentialBranchPoint   => recursivelyProcess( sequentialBranchPoint   , vertex)
      case shorthandBranchGraft    : ShorthandBranchGraft    => recursivelyProcess( shorthandBranchGraft    , vertex)
      case shorthandConfigVariable : ShorthandConfigVariable => recursivelyProcess( shorthandConfigVariable , vertex)
      case shorthandTaskVariable   : ShorthandTaskVariable   => recursivelyProcess( shorthandTaskVariable   , vertex)
      case submitterDef            : SubmitterDef            => recursivelyProcess( submitterDef            , vertex)
      case summaryDef              : SummaryDef              => recursivelyProcess( summaryDef              , vertex)
      case summaryOfDef            : SummaryOfDef            => recursivelyProcess( summaryOfDef            , vertex)
      case taskDef                 : TaskDef                 => recursivelyProcess( taskDef                 , vertex)
      case taskHeader              : TaskHeader              => recursivelyProcess( taskHeader              , vertex)
      case taskInputs              : TaskInputs              => recursivelyProcess( taskInputs              , vertex)
      case taskInputSpec           : TaskInputSpec[RValue]   => recursivelyProcess( taskInputSpec           , vertex)
      case taskOutputs             : TaskOutputs             => recursivelyProcess( taskOutputs             , vertex)
      case taskOutputSpec          : TaskOutputSpec[RValue]  => recursivelyProcess( taskOutputSpec          , vertex)
      case taskPackageNames        : TaskPackageNames        => recursivelyProcess( taskPackageNames        , vertex)
      case taskParams              : TaskParams              => recursivelyProcess( taskParams              , vertex)
      case taskParamSpec           : TaskParamSpec[RValue]   => recursivelyProcess( taskParamSpec           , vertex)
      case taskVariable            : TaskVariable            => recursivelyProcess( taskVariable            , vertex)
      case unbound                 : Unbound                 => recursivelyProcess( unbound                 , vertex)
      case versionerDef            : VersionerDef            => recursivelyProcess( versionerDef            , vertex)
      case workflowDefinition      : WorkflowDefinition      => recursivelyProcess( workflowDefinition      , vertex)
    }
  }

  private def recursivelyProcess( actionDef               : ActionDef                , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( baselineBlockDef        : BaselineBlockDef         , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( branchBlockDef          : BranchBlockDef           , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( branchBlockBlock        : BranchPointBlock         , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( bashCode                : BashCode                 , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(bashCode, vertex)
  }

  private def recursivelyProcess( branchGraft             : BranchGraft              , vertex:Vertex) : Seq[Vertex] = {
    val newVertex = new BranchGraftVertex(branchGraft)
    Edge.connect(newVertex, vertex)

    val children = processChildren(branchGraft, vertex)

    return Seq(newVertex) ++ children
  }

  private def recursivelyProcess( branchGraftElement      : BranchGraftElement       , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(branchGraftElement, vertex)
  }

  private def recursivelyProcess( branchPointDef          : BranchPointDef           , vertex:Vertex) : Seq[Vertex] = {
    val branchPointDefVertex = new BranchPointDefVertex(branchPointDef)
    Edge.connect(branchPointDefVertex, vertex)

    val branchVerticesAndChildren = processChildren(branchPointDef, branchPointDefVertex)

    return Seq(branchPointDefVertex) ++ branchVerticesAndChildren
  }

  private def recursivelyProcess( branchPointRef          : BranchPointRef           , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( branchSpec              : BranchSpec[RValue]       , vertex:Vertex) : Seq[Vertex] = {

    val branchVertex = new BranchSpecVertex(branchSpec)
    Edge.connect(branchVertex, vertex)

    val children = processChildren(branchSpec, branchVertex)

    return Seq(branchVertex) ++ children
  }

  private def recursivelyProcess( callDefinition          : CallDefinition           , vertex:Vertex) : Seq[Vertex] = {
    // By this point, a concrete TaskDefinition should have already been created for every CallDefinition object.
    //
    // Therefore, we can now safely ignore any CallDefinition objects we encounter.
    return Seq()
  }

  private def recursivelyProcess( comments                : Comments                 , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(comments, vertex)
  }

  private def recursivelyProcess( configAssignment        : ConfigAssignment         , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(configAssignment, vertex)
  }

  private def recursivelyProcess( configDefinition        : ConfigDefinition         , vertex:Vertex) : Seq[Vertex] = {

    val currentVertex = new ConfigDefinitionVertex(configDefinition)

    val children = processChildren(configDefinition, currentVertex)

    return Seq(currentVertex) ++ children
  }

  private def recursivelyProcess( configParamSpec         : ConfigParamSpec[RValue]  , vertex:Vertex) : Seq[Vertex] = {
    val paramVertex = new ConfigParamVertex(configParamSpec)
    Edge.connect(paramVertex, vertex)

    val children = processChildren(configParamSpec, paramVertex)

    return Seq(paramVertex) ++ children
  }

  private def recursivelyProcess( configVariable          : ConfigVariable           , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(configVariable, vertex)
  }

  private def recursivelyProcess( crossProduct            : CrossProduct             , vertex:Vertex) : Seq[Vertex] = {

    val crossProductVertex = new CrossProductVertex(crossProduct)
    Edge.connect(crossProductVertex, vertex)
    return Seq(crossProductVertex)

  }

  private def recursivelyProcess( funcDef                 : FuncDef                  , vertex:Vertex) : Seq[Vertex] = {
    // By this point, a concrete TaskDefinition should have already been created for every CallDefinition object.
    //
    // Therefore, we can now safely ignore any FuncDef objects we encounter.
    return Seq()
  }

  private def recursivelyProcess( groupDefinition         : GroupDef                 , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( taskInputSpec           : TaskInputSpec[RValue]    , vertex:Vertex) : Seq[Vertex] = {

    val inputVertex = new TaskInputVertex(taskInputSpec)
    Edge.connect(inputVertex, vertex)

    val children = processChildren(taskInputSpec, inputVertex)

    return Seq(inputVertex) ++ children
  }

  private def recursivelyProcess( literal                 : Literal                  , vertex:Vertex) : Seq[Vertex] = {

    val literalVertex = new LiteralVertex(literal)
    Edge.connect(literalVertex, vertex)

    return Seq(literalVertex)
  }

  private def recursivelyProcess( taskOutputSpec          : TaskOutputSpec[RValue]   , vertex:Vertex) : Seq[Vertex] = {

    val outputVertex = new TaskOutputVertex(taskOutputSpec)
    Edge.connect(vertex, outputVertex)

    val children = processChildren(taskOutputSpec, outputVertex)

    return Seq(outputVertex) ++ children
  }

  private def recursivelyProcess( paramSpec               : TaskParamSpec[RValue]    , vertex:Vertex) : Seq[Vertex] = {
    val paramVertex = new TaskParamVertex(paramSpec)
    Edge.connect(paramVertex, vertex)

    val children = processChildren(paramSpec, paramVertex)

    return Seq(paramVertex) ++ children
  }

  private def recursivelyProcess( packageDef              : PackageDef               , vertex:Vertex) : Seq[Vertex] = {
    val packageVertex = new PackageVertex(packageDef)

    val children = processChildren(packageDef, packageVertex)

    return Seq(packageVertex) ++ children
  }

  private def recursivelyProcess( packageSpec             : PackageSpec              , vertex:Vertex) : Seq[Vertex] = {

    val packageVertex = new PackageSpecVertex(packageSpec)
    Edge.connect(packageVertex, vertex)

    val children = processChildren(packageSpec, packageVertex)

    return Seq(packageVertex) ++ children
  }

  private def recursivelyProcess( planDefinition          : PlanDefinition           , vertex:Vertex) : Seq[Vertex] = {
    val planVertex = new PlanDefinitionVertex(planDefinition)

    val children = processChildren(planDefinition, planVertex)

    return Seq(planVertex) ++ children
  }

  private def recursivelyProcess( sequence                : Sequence                 , vertex:Vertex) : Seq[Vertex] = {

    return justProcessChildren(sequence, vertex)

  }

  private def recursivelyProcess( sequentialBranchPoint   : SequentialBranchPoint    , vertex:Vertex) : Seq[Vertex] = {

    val branchPointVertex = new SequentialBranchPointVertex(sequentialBranchPoint)
    Edge.connect(branchPointVertex, vertex)

    val branchVertices = recursivelyProcess(sequentialBranchPoint.sequence, branchPointVertex)

    return Seq(branchPointVertex) ++ branchVertices

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

  private def recursivelyProcess( submitterDef            : SubmitterDef             , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( summaryDef              : SummaryDef               , vertex:Vertex) : Seq[Vertex] = {
    val summaryVertex = new SummaryVertex(summaryDef)
    val children = processChildren(summaryDef, summaryVertex)
    return Seq(summaryVertex) ++ children
  }

  private def recursivelyProcess( summaryOfDef            : SummaryOfDef             , vertex:Vertex) : Seq[Vertex] = {
    val summaryOfVertex = new SummaryOfVertex(summaryOfDef)

    val children = processChildren(summaryOfDef, summaryOfVertex)

    children.foreach({ child =>
      child match {
        case outputVertex:TaskOutputVertex => {
          Edge.connect(outputVertex, vertex)
        }
        case _ => /* This space intentionally left blank */
      }
    })

    return Seq(summaryOfVertex) ++ children
  }

  private def recursivelyProcess( taskDef                 : TaskDef                  , vertex:Vertex) : Seq[Vertex] = {
    val taskVertex = new TaskVertex(taskDef)

    val children = processChildren(taskDef, taskVertex)

    return Seq(taskVertex) ++ children
  }

  private def recursivelyProcess( taskHeader              : TaskHeader               , vertex:Vertex) : Seq[Vertex] = {

    return justProcessChildren(taskHeader, vertex)
  }

  private def recursivelyProcess( taskInputs              : TaskInputs               , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(taskInputs, vertex)
  }

  private def recursivelyProcess( taskOutputs             : TaskOutputs              , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(taskOutputs, vertex)
  }

  private def recursivelyProcess( taskPackageNames        : TaskPackageNames         , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(taskPackageNames, vertex)
  }

  private def recursivelyProcess( taskParams              : TaskParams               , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(taskParams, vertex)
  }

  private def recursivelyProcess( taskVariable            : TaskVariable             , vertex:Vertex) : Seq[Vertex] = {
    val newVertex = new TaskVariableVertex(taskVariable)
    Edge.connect(newVertex, vertex)
    return Seq(newVertex)
  }

  private def recursivelyProcess( unbound                 : Unbound                  , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(unbound, vertex)
  }

  private def recursivelyProcess( versionerDef            : VersionerDef             , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( workflowDefinition      : WorkflowDefinition       , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(workflowDefinition, vertex)
  }


  private def justProcessChildren(astNode:ASTType, vertex:Vertex) : Seq[Vertex] = {

    // For the purposes of converting an AST into a packed graph, certain AST nodes do not generate a vertex
    //
    // We denote this by creating a new empty sequence
    val current : Seq[Vertex] = Seq()

    // Additionally, we expect that such a node
    // will return an empty sequence when asked for its children.
    //
    // As such, it is expected that this method should return an empty sequence of vertices.
    //
    // However, in the interest of ensuring a complete traversal of the AST,
    // we recursively process the children of the current node of the AST,
    // even when we expect it to be empty.
    val children = processChildren(astNode, vertex)

    return current ++ children
  }

  private def processChildren(astNode:ASTType, vertex:Vertex) : Seq[Vertex] = {
    return astNode.children.flatMap({ child:ASTType => recursivelyProcessAST(child, vertex)})
  }

}
