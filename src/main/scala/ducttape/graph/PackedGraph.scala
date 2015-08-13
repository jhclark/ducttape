package ducttape.graph

import java.io.File

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.FileFormatException
import ducttape.syntax.Namespace

import grizzled.slf4j.Logging

class PackedGraph(val wd: WorkflowDefinition) extends Graph {

  val vertices = Vertices_to_PackedGraph.process(wd)

  System.out.println(Vertices_to_Graphviz.latexPoster(vertices))

}

object Vertices_to_Graphviz {

  private object IdentifierTracker {

    private val map = new java.util.IdentityHashMap[Object,String]()

    var counter = 0

    def getID(item:Object, name:String="vertex") : String = {

      if (map.containsKey(item)) {

        return map.get(item)

      } else {

        counter += 1

        val id = s"${name}${counter}"

        map.put(item,id)

        return id

      }

    }

    def hasNeverSeen(item:Object) : Boolean = !map.containsKey(item)

  }

  private def drawNode(vertex:Vertex, s:StringBuilder) : Unit = {
    s.append(s"""\t${IdentifierTracker.getID(vertex)} [style="${vertex.getClass.getSimpleName}" label="${vertex.id}"]\n""")
  }

  private def processTaskSubgraph[Contents<:TaskLike](task:TaskLikeVertex[Contents], processFunction:((SpecVertex, String)=>Unit)) : Unit = {
    task.foreachParent( (parent:Vertex) => {
      parent match {
        case input: TaskInputVertex   => processFunction(input, "TaskInputVertex")
        case param: TaskParamVertex   => processFunction(param, "TaskParamVertex")
        case pkg:   PackageSpecVertex => processFunction(pkg,   "PackageSpecVertex")
        case _                        => throw new RuntimeException("Every parent of a TaskVertex should be either a TaskInputVertex or a TaskParamVertex, but a different type was found: %s with class %s".format(parent, parent.getClass.getSimpleName))
      }
    })

    task.foreachChild( (parent:Vertex) => {
      parent match {
        case output: TaskOutputVertex  => processFunction(output, "TaskOutputVertex")
        case pkg:    PackageSpecVertex => /* This space intentionally left blank. */
        case _                         => throw new RuntimeException("Every child of a TaskVertex should be a TaskOutputVertex, but a different type was found: %s".format(parent))
      }
    })
  }

  private def drawTaskSubgraphBox[Contents<:TaskLike](task:TaskLikeVertex[Contents], s:StringBuilder) : Unit = {
    def process(vertex:SpecVertex, specType:String) {
      s.append('(').append(IdentifierTracker.getID(vertex)).append(')')
    }

    s.append("""\node[fit=""")
    s.append('(').append(IdentifierTracker.getID(task)).append(')')
    processTaskSubgraph(task, process)
    s.append(""", draw, thick, inner sep=1.0cm""")
    if      (task.contents.keyword=="task")    s.append(""", black""")
    else if (task.contents.keyword=="package") s.append(""", gray, rounded corners""")
    s.append("""] {};""").append('\n')
  }

  private def drawTaskSubgraph(task:TaskVertex, s:StringBuilder) : Unit = {

    val taskID = IdentifierTracker.getID(task)

    s.append(s"""\tsubgraph cluster_${taskID} {\n""")
    s.append(s"""\t\t${taskID} [style="task" label="${task.id}"]\n""")

    def process(vertex:SpecVertex, specType:String) {
    	s.append(s"""\t\t${IdentifierTracker.getID(vertex)} [style="${specType}" label="${vertex.id}"];\n""")
    }

    processTaskSubgraph(task, process)

    s.append("\t}\n\n")

  }

  private def drawArrow(from:Vertex, to:Vertex, s:StringBuilder) : Unit = {
    s.append(s"""\t${IdentifierTracker.getID(from)} -> ${IdentifierTracker.getID(to)};\n""")
  }


  val unseenVertices : PartialFunction[Vertex,Vertex] = { case vertex:Vertex if IdentifierTracker.hasNeverSeen(vertex) => vertex }
  val unseenEdges    : PartialFunction[Edge,Edge]     = { case   edge:Edge   if IdentifierTracker.hasNeverSeen(edge)   => edge }

  def graph(vertices: Seq[Vertex], s:StringBuilder=new StringBuilder()) : String = {

    s.append("digraph G {\n\n") // Start directed graph G

    s.append("\tgraph [nodesep=\"0.7\", ranksep=\"0.7\"];\n\n")

    vertices.collect({case task:TaskVertex => task}).foreach({task:TaskVertex => drawTaskSubgraph(task, s)})
    s.append("\n")

    vertices.collect(unseenVertices).foreach({vertex:Vertex => drawNode(vertex, s)})
    s.append("\n")

    vertices.foreach({ vertex:Vertex =>
      vertex.foreachEdge({ edge =>
        if (IdentifierTracker.hasNeverSeen(edge)) {
          IdentifierTracker.getID(edge, "edge")
          drawArrow(edge.from, edge.to, s)
        }
      })
    })
    s.append("\n")

    s.append("}\n\n") // End directed graph G

    return s.toString()

  }


  def dot2tex(vertices: Seq[Vertex], s:StringBuilder=new StringBuilder()) : String = {

    s.append("""\begin{center}""").append('\n')
    s.append("""\begin{tikzpicture}[>=latex, scale=0.85, transform shape]""").append('\n')
    s.append("""\begin{dot2tex}[dot,scale=0.85,tikzedgelabels,codeonly]""").append("\n\n")

    graph(vertices, s)

    s.append("""\end{dot2tex}""").append('\n')
    s.append('%').append('\n')
    vertices.collect({case task:TaskVertex => task}).foreach({task:TaskVertex => drawTaskSubgraphBox(task, s)})
    vertices.collect({case task:PackageVertex => task}).foreach({task:PackageVertex => drawTaskSubgraphBox(task, s)})
    s.append('%').append('\n')
    s.append("""\end{tikzpicture}""").append('\n')
    s.append("""\end{center}""").append('\n')

    return s.toString()

  }

  def latexPoster(vertices: Seq[Vertex], s:StringBuilder=new StringBuilder()) : String = {

    s.append("""\documentclass[a0,landscape]{sciposter}""").append('\n')
    s.append('\n')
    //
    // Scala has a bug, which has been resolved as "won't fix",
    // wherein the sequence 'backslash u' is interpreted as a Unicode escape,
    // *even* inside triple-quoted strings.
    //
    // (This also happens inside comments, which is why 'backslash u' was written out in words inside this comment)
    //
    // See https://issues.scala-lang.org/browse/SI-4706
    //
    s.append("""\""").append("""usepackage[margin=1in]{geometry}""").append('\n')
    s.append("""\""").append("""usepackage[forceshell,outputdir={auto_generated/}]{dot2texi}""").append('\n')
    s.append("""\""").append("""usepackage{tikz}""").append('\n')
    s.append("""\""").append("""usetikzlibrary{shapes,arrows,shadows,shadows.blur,positioning,fit}""").append('\n')
    //
    s.append('\n')
    s.append("""\renewcommand{\normalsize}{\fontsize{12}{12}\selectfont}""").append('\n')
    s.append('\n')
    s.append("""% Note: When compiling this document using TeXShop on Mac OS X,""").append('\n')
    s.append("""%       if dot2tex is installed using fink, the following workaround can be used""").append('\n')
    s.append("""%       to ensure that TeXShop can find dot2tex""").append('\n')
    s.append("""%""").append('\n')
    s.append("""%       sudo ln -s /sw/bin/dot2tex /usr/texbin/dot2tex""").append('\n')
    s.append('\n')
    s.append('\n')
//    s.append("""\title{A workflow management acid test}""").append('\n')
//    s.append('\n')
//    s.append("""\author{Lane Schwartz \textnormal{and} Jonathan Clark}""").append('\n')
//    s.append('\n')
//    s.append("""\institute{University of Illinois at Urbana-Champaign}""").append('\n')
//    s.append('\n')
//    s.append("""\date{}""").append('\n')
    s.append('\n')
    s.append("""\definecolor{darkpastelgreen}{rgb}{0.01, 0.75, 0.24}""").append('\n')
    s.append("""\definecolor{darkmagenta}{rgb}{0.55, 0.0, 0.55}""").append('\n')
    s.append("""\definecolor{darkgray}{rgb}{0.66, 0.66, 0.66}""").append('\n')
    s.append("""\definecolor{deepchampagne}{rgb}{0.98, 0.84, 0.65}""").append('\n')
    s.append("""\definecolor{deepskyblue}{rgb}{0.0, 0.75, 1.0}""").append('\n')
    s.append('\n')
    s.append("""\pgfdeclarelayer{background}""").append('\n')
    s.append("""\pgfdeclarelayer{foreground}""").append('\n')
    s.append("""\pgfsetlayers{background,main,foreground}""").append('\n')
    s.append('\n')
    s.append("""\tikzstyle{BranchSpecVertex} = [ellipse, draw=none, inner sep=0.3mm, fill=blue, drop shadow, text centered, anchor=north, text=white]""").append('\n')
    s.append("""\tikzstyle{task}   = [rectangle, draw=none, rounded corners=2mm, fill=orange, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]""").append('\n')
    s.append("""\tikzstyle{PackageVertex}   = [rectangle, draw=none, rounded corners=2mm, fill=orange, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]""").append('\n')
    s.append("""\tikzstyle{ConfigDefinitionVertex}   = [rectangle, draw=none, rounded corners=2mm, fill=deepchampagne, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]""").append('\n')
    s.append("""\tikzstyle{BranchPointDefVertex} = [rectangle, draw=none, fill=red, drop shadow, text centered, anchor=north, text=white]""").append('\n')
    s.append("""\tikzstyle{SequentialBranchPointVertex} = [rectangle, draw=none, fill=red, drop shadow, text centered, anchor=north, text=white]""").append('\n')
    s.append('\n')
    s.append("""\tikzstyle{graft}  = [fill=blue!20]""").append('\n')
    s.append('\n')
    s.append("""\tikzstyle{PackageSpecVertex} = [rectangle,rounded corners]""").append('\n')
    s.append("""\tikzstyle{TaskParamVertex}   = [rectangle]""").append('\n')
    s.append("""\tikzstyle{TaskInputVertex}   = [ellipse]""").append('\n')
    s.append("""\tikzstyle{TaskOutputVertex}  = [ellipse]""").append('\n')
    s.append('\n')
    s.append('\n')
    s.append('\n')
    s.append("""\tikzstyle{file} = [ellipse, draw, inner sep=0.3mm, fill=darkpastelgreen, text centered, anchor=north, text=white]""").append('\n')
    s.append("""\tikzstyle{LiteralVertex} = [rectangle, draw, inner sep=0.3mm, fill=darkpastelgreen, text centered, anchor=north, text=white]""").append('\n')
    s.append("""\tikzstyle{TaskVariableVertex} = [rectangle, draw, inner sep=0.3mm, fill=darkmagenta, text centered, anchor=north, text=white]""").append('\n')
    s.append("""\tikzstyle{ConfigParamVertex} = [rectangle]""").append('\n')
    s.append("""\tikzstyle{PlanDefinitionVertex} = [rectangle, draw, inner sep=0.3mm, fill=darkgray, text centered, anchor=north, text=white]""").append('\n')
    s.append("""\tikzstyle{BranchGraftVertex} = [rectangle, draw, inner sep=0.3mm, fill=darkmagenta, text centered, anchor=north, text=white]""").append('\n')
    s.append("""\tikzstyle{RootVertex} = [rectangle, draw, inner sep=0.3mm, fill=deepskyblue, text centered, anchor=north, text=white]""").append('\n')
    s.append('\n')
    s.append('\n')
    s.append("""\begin{document}""").append('\n')
//    s.append("""\maketitle""").append('\n')
    s.append('\n')
    s.append('\n')
//    s.append("""\section{Entire packed graph}""").append('\n')
    dot2tex(vertices, s)
    s.append("""\end{document}""").append('\n')
    return s.toString()
  }

}

object Vertices_to_PackedGraph {

//	val isTaskVertex   : PartialFunction[Vertex,TaskVertex]        = { case vertex:TaskVertex        => vertex }
//  val isConfigVertex : PartialFunction[Vertex,ConfigParamVertex] = { case vertex:ConfigParamVertex => vertex }
//
//  val isInputVertex  : PartialFunction[Vertex,TaskInputVertex]   = { case vertex:TaskInputVertex   => vertex }
//  val isParamVertex  : PartialFunction[Vertex,TaskParamVertex]   = { case vertex:TaskParamVertex   => vertex }
//  val isOutputVertex : PartialFunction[Vertex,TaskOutputVertex]  = { case vertex:TaskOutputVertex  => vertex }

  val isTaskSpecVertex : PartialFunction[Vertex,TaskSpecVertex] = { case vertex:TaskSpecVertex => vertex }

  val isVariableRef    : PartialFunction[Vertex, VariableReferenceVertex] = { case vertex:VariableReferenceVertex => vertex }

  val isPackageSpec    : PartialFunction[Vertex, PackageSpecVertex] = { case vertex:PackageSpecVertex => vertex }
  val isPackageVertex  : PartialFunction[Vertex, PackageVertex] = { case vertex:PackageVertex => vertex }

  private def createMap[V <: Vertex](vertices:Seq[Vertex], selectionFunction:PartialFunction[Vertex,V]) : Map[String,Set[V]] = {

    val subsetOfVertices = vertices.collect(selectionFunction)

    val names = subsetOfVertices.map{ vertex => vertex.toString() }

    val tuples = names.zip(subsetOfVertices)
    val result : Map[String,Set[V]] = {

      import scala.collection.mutable.{HashMap, Set, MultiMap}

      val mutableMultiMap = new HashMap[String, Set[V]] with MultiMap[String, V]

      tuples.foreach({ case (name, vertex) =>
        mutableMultiMap.addBinding(name, vertex)
      })

      mutableMultiMap.
        // convert each value from a mutable set to an immutable set
        map({case (key:String, value) => (key, value.toSet)}).
        // convert the mutable map to an immutable map
        toMap
    }

    return result

  }


  private def addEdges[FromVertex <: Vertex, ToVertex <: Vertex]
                      (vertices:Seq[Vertex],
                          functionToSelectFromVertices:PartialFunction[Vertex,FromVertex],
                          functionToSelectToVertices:PartialFunction[Vertex,ToVertex]
                      ) : Unit = {

    val fromMap : Map[String,Set[FromVertex]] = createMap(vertices, functionToSelectFromVertices)
    val toMap   : Map[String,Set[ToVertex]]   = createMap(vertices, functionToSelectToVertices)

    toMap.foreach({ case (key:String, valueSet:Set[ToVertex]) => {
      valueSet.foreach({ toVertex:ToVertex =>
        fromMap.get(key) match {
          case Some(fromVertexSet) => {
            if (fromVertexSet.size==1) {
              val fromVertex = fromVertexSet.seq.head
              Edge.connect(fromVertex, toVertex)
            } else {
              throw new RuntimeException("Expected exactly one parent vertex matching name %s, but found %d".format(key, fromVertexSet.size))
            }
          }
          case None => throw new RuntimeException("No parent vertex found for reference %s".format(key))
        }
      })
    }})
  }


  def process(astNode:WorkflowDefinition) : Seq[Vertex] = {
    val vertices = AST_to_Vertices.recursivelyProcessAST(astNode)

    addEdges(vertices, isTaskSpecVertex, isVariableRef)
    addEdges(vertices, isPackageVertex, isPackageSpec)

    return vertices
  }

}


/**
 * Collection of recursive utility functions to transform an abstract syntax tree into a partially connected graph.
 *
 * @author Lane Schwartz
 */
object AST_to_Vertices {

  def recursivelyProcessAST(astNode:WorkflowDefinition) : Seq[Vertex] = {
    val rootVertex = new RootVertex()
    val rest = recursivelyProcessAST(astNode, rootVertex)
    return Seq(rootVertex) ++ rest
  }

  private def recursivelyProcessAST(astNode:ASTType, vertex:Vertex) : Seq[Vertex] = {
    astNode match {
      case actionDef               : ActionDef               => recursivelyProcess( actionDef               , vertex)
      case baselineBlockDef        : BaselineBlockDef        => recursivelyProcess( baselineBlockDef        , vertex)
      case branchBlockDef          : BranchBlockDef          => recursivelyProcess( branchBlockDef          , vertex)
      case bashCode                : BashCode                => recursivelyProcess( bashCode                , vertex)
      case branchGraft             : BranchGraft             => recursivelyProcess( branchGraft             , vertex)
      case branchGraftElement      : BranchGraftElement      => recursivelyProcess( branchGraftElement      , vertex)
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
      case groupDefinition         : GroupDefinition         => recursivelyProcess( groupDefinition         , vertex)
      case literal                 : Literal                 => recursivelyProcess( literal                 , vertex)
      case packageDef              : PackageDef              => recursivelyProcess( packageDef              , vertex)
      case packageSpec             : PackageSpec             => recursivelyProcess( packageSpec             , vertex)
      case planDefinition          : PlanDefinition          => recursivelyProcess( planDefinition          , vertex)
      case sequence                : Sequence                => recursivelyProcess( sequence                , vertex)
      case sequentialBranchPoint   : SequentialBranchPoint   => recursivelyProcess( sequentialBranchPoint   , vertex)
      case shorthandBranchGraft    : ShorthandBranchGraft    => recursivelyProcess( shorthandBranchGraft    , vertex)
      case shorthandConfigVariable : ShorthandConfigVariable => recursivelyProcess( shorthandConfigVariable , vertex)
      case shorthandTaskVariable   : ShorthandTaskVariable   => recursivelyProcess( shorthandTaskVariable   , vertex)
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
    ???
  }

  private def recursivelyProcess( funcDef                 : FuncDef                  , vertex:Vertex) : Seq[Vertex] = {
    // By this point, a concrete TaskDefinition should have already been created for every CallDefinition object.
    //
    // Therefore, we can now safely ignore any FuncDef objects we encounter.
    return Seq()
  }

  private def recursivelyProcess( groupDefinition         : GroupDefinition          , vertex:Vertex) : Seq[Vertex] = {
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
    return Seq(new PlanDefinitionVertex(planDefinition))
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

  private def recursivelyProcess( summaryOfDef            : SummaryOfDef             , vertex:Vertex) : Seq[Vertex] = {
    ???
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

