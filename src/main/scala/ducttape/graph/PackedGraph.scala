package ducttape.graph

import java.io.File

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.FileFormatException
import ducttape.syntax.Namespace

import grizzled.slf4j.Logging

class PackedGraph(val wd: WorkflowDefinition) extends Graph {

//  val vertices = AST_to_Vertices.recursivelyProcessAST(wd)

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

  private def processTaskSubgraph(task:TaskVertex, processFunction:((TaskSpecVertex, String)=>Unit)) : Unit = {
    task.foreachParent( (parent:Vertex) => {
      parent match {
        case input: TaskInputVertex => processFunction(input, "input")
        case param: TaskParamVertex => processFunction(param, "param")
        case _                      => throw new RuntimeException("Every parent of a TaskVertex should be either a TaskInputVertex or a TaskParamVertex, but a different type was found: %s".format(parent))
      }
    })

    task.foreachChild( (parent:Vertex) => {
      parent match {
        case output: TaskOutputVertex => processFunction(output, "output")
        case _                        => throw new RuntimeException("Every child of a TaskVertex should be a TaskOutputVertex, but a different type was found: %s".format(parent))
      }
    })
  }

  private def drawTaskSubgraphBox(task:TaskVertex, s:StringBuilder) : Unit = {
    def process(vertex:TaskSpecVertex, specType:String) {
      s.append('(').append(IdentifierTracker.getID(vertex)).append(')')
    }

    s.append("""\node[fit=""")
    processTaskSubgraph(task, process)
    s.append(""", draw, thick, inner sep=1.0cm, black] {};""").append('\n')
  }

  private def drawTaskSubgraph(task:TaskVertex, s:StringBuilder) : Unit = {

    val taskID = IdentifierTracker.getID(task)

    s.append(s"""\tsubgraph cluster_${taskID} {\n""")
    s.append(s"""\t\t${taskID} [style="task" label="${task.id}"]\n""")

    def process(vertex:TaskSpecVertex, specType:String) {
    	s.append(s"""\t\t${IdentifierTracker.getID(vertex)} [style="${specType}" label="${vertex.id}"];\n""")
    }

    processTaskSubgraph(task, process)
//    task.foreachParent( (parent:Vertex) => {
//      parent match {
//        case input: TaskInputVertex => process(input, "input")
//        case param: TaskParamVertex => process(param, "param")
//        case _                      => throw new RuntimeException("Every parent of a TaskVertex should be either a TaskInputVertex or a TaskParamVertex, but a different type was found: %s".format(parent))
//      }
//    })
//
//    task.foreachChild( (parent:Vertex) => {
//      parent match {
//        case output: TaskOutputVertex => process(output, "output")
//        case _                        => throw new RuntimeException("Every child of a TaskVertex should be a TaskOutputVertex, but a different type was found: %s".format(parent))
//      }
//    })

    s.append("\t}\n\n")

  }

  private def drawArrow(from:Vertex, to:Vertex, s:StringBuilder) : Unit = {
    s.append(s"""\t${IdentifierTracker.getID(from)} -> ${IdentifierTracker.getID(to)};\n""")
  }

//  private def drawTaskArrows(task:TaskVertex, s:StringBuilder) : Unit = {
//
//    val taskID = taskIDs.getID(task)
//
//    var outputIDs = new IdentifierTracker(s"${taskID}_out", map)
//    var inputIDs = new IdentifierTracker(s"${taskID}_in", map)
//    var paramIDs = new IdentifierTracker(s"${taskID}_param", map)
//
//    def process(specs:Specs, counter:IdentifierTracker, inputOrParam:Boolean) {
//      specs.specs.foreach( (spec:Spec) => {
//        if (inputOrParam) {
//          s.append(s"""\t${counter.getID(spec)} -> ${taskID};\n""")
//        } else {
//          s.append(s"""\t${taskID} -> ${counter.getID(spec)};\n""")
//        }
//      })
//    }
//
//    task.header.children.foreach( (specs:Specs) => {
//      specs match {
//      case inputs:      TaskInputs       => process(inputs,  inputIDs,  true)
//      case outputs:     TaskOutputs      => process(outputs, outputIDs, false)
//      case params:      TaskParams       => process(params,  paramIDs,  true)
//      case packageNames:TaskPackageNames => {}
//      }
//    })
//
//    s.append("\n")
//  }

  val unseenVertices : PartialFunction[Vertex,Vertex] = { case vertex:Vertex if IdentifierTracker.hasNeverSeen(vertex) => vertex }
  val unseenEdges    : PartialFunction[Edge,Edge]     = { case   edge:Edge   if IdentifierTracker.hasNeverSeen(edge)   => edge }

  def graph(vertices: Seq[Vertex], s:StringBuilder=new StringBuilder()) : String = {

    s.append("digraph G {\n\n") // Start directed graph G

    s.append("\tgraph [nodesep=\"1\", ranksep=\"1\"];\n\n")

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
    s.append('%').append('\n')
    s.append("""\end{tikzpicture}""").append('\n')
    s.append("""\end{center}""").append('\n')

    return s.toString()

  }

  def latexPoster(vertices: Seq[Vertex], s:StringBuilder=new StringBuilder()) : String = {

    s.append("""\documentclass[a0]{sciposter}""").append('\n')
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
//    s.append("""\""").append("""usepackage{fancyvrb}""").append('\n')
//    s.append("""\""").append("""usepackage{latexsym}""").append('\n')
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
    s.append("""\tikzstyle{ConfigDefinitionVertex}   = [rectangle, draw=none, rounded corners=2mm, fill=deepchampagne, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]""").append('\n')
    s.append("""\tikzstyle{BranchPointDefVertex} = [rectangle, draw=none, fill=red, drop shadow, text centered, anchor=north, text=white]""").append('\n')
    s.append("""\tikzstyle{SequentialBranchPointVertex} = [rectangle, draw=none, fill=red, drop shadow, text centered, anchor=north, text=white]""").append('\n')
    s.append('\n')
    s.append("""\tikzstyle{graft}  = [fill=blue!20]""").append('\n')
    s.append('\n')
    s.append("""\tikzstyle{param}   = [rectangle]""").append('\n')
    s.append("""\tikzstyle{input}   = [ellipse]""").append('\n')
    s.append("""\tikzstyle{output}   = [ellipse]""").append('\n')
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

  val isVariableRef  : PartialFunction[Vertex, VariableReferenceVertex] = { case vertex:VariableReferenceVertex => vertex }

  private def createMap[V <: Vertex](vertices:Seq[Vertex], selectionFunction:PartialFunction[Vertex,V]) : Map[String,Set[V]] = {

    val subsetOfVertices = vertices.collect(selectionFunction)

    val names = subsetOfVertices.map{ vertex => vertex.toString() }

    val tuples = names.zip(subsetOfVertices)
//    tuples.sortBy(_._1).foreach({ case (key:String, value) => {
//       System.err.println(key)
//    }})
//    System.err.println("====")
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

//    result.foreach({ case (key:String, valueSet:Set[V]) =>
//      System.err.println(s"${key} =>")
//      valueSet.foreach( (value:V) =>
//        System.err.println(s"\t${value}")
//      )
//    })
//    System.err.println("**********************************************")

    return result

    //return subsetOfVertices.zip(names).toMap

  }

  def process(astNode:WorkflowDefinition) : Seq[Vertex] = {
    val vertices = AST_to_Vertices.recursivelyProcessAST(astNode)

//    val taskVertices   = vertices.collect(isTaskVertex)
//    val configVertices = vertices.collect(isConfigVertex)

    val taskSpecsMap    : Map[String,Set[TaskSpecVertex]]          = createMap(vertices, isTaskSpecVertex)
    val variableRefsMap : Map[String,Set[VariableReferenceVertex]] = createMap(vertices, isVariableRef)

//    val params = createMap(vertices, isParamVertex)
//    val outputs = createMap(vertices, isOutputVertex)

//    taskSpecsMap.foreach({ case (key:String, value:TaskSpecVertex) => {
//      System.err.println(key)
//    }})
//    System.err.println("==============================")

    variableRefsMap.foreach({ case (key:String, valueSet:Set[VariableReferenceVertex]) => {
      valueSet.foreach({ variableRefVertex:VariableReferenceVertex =>
        taskSpecsMap.get(key) match {
          case Some(taskVertexSet) => {
            if (taskVertexSet.size==1) {
              val taskVertex = taskVertexSet.seq.head
            	Edge.connect(taskVertex, variableRefVertex)
            } else {
            	throw new RuntimeException("Expected exactly one task spec matching name %s, but found %d".format(key, taskVertexSet.size))
            }
          }
          case None => throw new RuntimeException("No task found for variable reference %s".format(key))
        }
      })
    }})
////      System.err.println(key)
//      taskSpecsMap.get(key) match {
//        case Some(taskVertex) => {
//          Edge.connect(taskVertex, key)
////          System.err.println("Foo")
//        }
//        case None       => throw new RuntimeException("No task found for variable reference %s".format(key))
//      }
//    }})

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
      case groupDefinition         : GroupDefinition         => recursivelyProcess( groupDefinition         , vertex)
      case literal                 : Literal                 => recursivelyProcess( literal                 , vertex)
      case packageSpec             : PackageSpec             => recursivelyProcess( packageSpec             , vertex)
      case planDefinition          : PlanDefinition          => recursivelyProcess( planDefinition          , vertex)
      case sequence                : Sequence                => recursivelyProcess( sequence                , vertex)
      case sequentialBranchPoint   : SequentialBranchPoint   => recursivelyProcess( sequentialBranchPoint   , vertex)
      case shorthandBranchGraft    : ShorthandBranchGraft    => recursivelyProcess( shorthandBranchGraft    , vertex)
      case shorthandConfigVariable : ShorthandConfigVariable => recursivelyProcess( shorthandConfigVariable , vertex)
      case shorthandTaskVariable   : ShorthandTaskVariable   => recursivelyProcess( shorthandTaskVariable   , vertex)
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

  private def recursivelyProcess( inputSpec               : TaskInputSpec[RValue]    , vertex:Vertex) : Seq[Vertex] = {

    val inputVertex = new TaskInputVertex(inputSpec)
    Edge.connect(inputVertex, vertex)

    val children = processChildren(inputSpec, inputVertex)

    return Seq(inputVertex) ++ children
  }
  private def recursivelyProcess( outputSpec              : TaskOutputSpec[RValue]   , vertex:Vertex) : Seq[Vertex] = {

    val outputVertex = new TaskOutputVertex(outputSpec)
    Edge.connect(vertex, outputVertex)

    val children = processChildren(outputSpec, outputVertex)

    return Seq(outputVertex) ++ children
  }
  private def recursivelyProcess( paramSpec               : TaskParamSpec[RValue]    , vertex:Vertex) : Seq[Vertex] = {
    val paramVertex = new TaskParamVertex(paramSpec)
    Edge.connect(paramVertex, vertex)

    val children = processChildren(paramSpec, paramVertex)

    return Seq(paramVertex) ++ children
  }
  private def recursivelyProcess( paramSpec               : ConfigParamSpec[RValue]  , vertex:Vertex) : Seq[Vertex] = {
    val paramVertex = new ConfigParamVertex(paramSpec)
    Edge.connect(paramVertex, vertex)

    val children = processChildren(paramSpec, paramVertex)

    return Seq(paramVertex) ++ children
  }
  private def recursivelyProcess( branchSpec              : BranchSpec[RValue]       , vertex:Vertex) : Seq[Vertex] = {

    val branchVertex = new BranchSpecVertex(branchSpec)
    Edge.connect(branchVertex, vertex)

    val children = processChildren(branchSpec, branchVertex)

    return Seq(branchVertex) ++ children
  }
  private def recursivelyProcess( packageSpec             : PackageSpec              , vertex:Vertex) : Seq[Vertex] = {

    val packageVertex = new PackageSpecVertex(packageSpec)
    Edge.connect(packageVertex, vertex)

    val children = processChildren(packageSpec, packageVertex)

    return Seq(packageVertex) ++ children
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
//      branchPointDef.children.flatMap({ spec:Spec =>
//
//      val branchVertex = new BranchVertex(spec)
//      Edge.connect(branchVertex, branchPointDefVertex)
//
//      val children = processChildren(spec, branchVertex)
//
//      Seq(branchVertex) ++ children
//    })

    return Seq(branchPointDefVertex) ++ branchVerticesAndChildren
  }

  private def recursivelyProcess( branchPointRef          : BranchPointRef           , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( callDefinition          : CallDefinition           , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( comments                : Comments                 , vertex:Vertex) : Seq[Vertex] = {
    return justProcessChildren(comments, vertex)
  }

  private def recursivelyProcess( configAssignment        : ConfigAssignment         , vertex:Vertex) : Seq[Vertex] = {
//
//    val lhsVertex = new ConfigParamVertex(configAssignment)
//    val children  = recursivelyProcessAST(configAssignment.spec.rval, lhsVertex)
//
//    return Seq(lhsVertex) ++ children
    return justProcessChildren(configAssignment, vertex)
  }

  private def recursivelyProcess( configDefinition        : ConfigDefinition         , vertex:Vertex) : Seq[Vertex] = {

    val currentVertex = new ConfigDefinitionVertex(configDefinition)

    val children = processChildren(configDefinition, currentVertex)

    return Seq(currentVertex) ++ children
  }

  private def recursivelyProcess( configVariable          : ConfigVariable           , vertex:Vertex) : Seq[Vertex] = {
//    val newVertex = new ConfigVariableVertex(configVariable)
//    Edge.connect(newVertex,vertex)
//    return Seq(newVertex)
    return processChildren(configVariable, vertex)
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

//    val branchValues = sequence.toSeq
//
//    val branchVertices = branchValues.flatMap({ number:BigDecimal =>
//
//      val string = number.toString()
//      val spec = new BranchSpec(string, new Literal(string))
//
//      val branchVertex = new BranchVertex(spec)
//      Edge.connect(branchVertex, vertex)
//
//      val children = recursivelyProcessAST(spec.rval, branchVertex)
//
//      Seq(branchVertex) ++ children
//
//    })
//
//    return branchVertices

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

  private def recursivelyProcess( taskDef                 : TaskDef                  , vertex:Vertex) : Seq[Vertex] = {
    val taskVertex = new TaskVertex(taskDef)

    val children = taskDef.children.flatMap({ child =>
      recursivelyProcessAST(child, taskVertex)
    })

    return Seq(taskVertex) ++ children
  }

  private def recursivelyProcess( taskHeader              : TaskHeader               , vertex:Vertex) : Seq[Vertex] = {

    val children = taskHeader.specsList.flatMap({ specs:ASTType => recursivelyProcessAST(specs, vertex)})

    return children
  }

  private def recursivelyProcess( taskInputs              : TaskInputs               , vertex:Vertex) : Seq[Vertex] = {
//    val inputVerticesAndChildren = taskInputs.specs.flatMap({ spec:Spec =>
//
//      val inputVertex = new TaskInputVertex(spec, taskInputs.comments.value)
//      Edge.connect(inputVertex, vertex)
//
//      val children = recursivelyProcessAST(spec, inputVertex)
//
//      Seq(inputVertex) ++ children
//    })
//
//    return inputVerticesAndChildren
    return justProcessChildren(taskInputs, vertex)
  }

  private def recursivelyProcess( taskOutputs             : TaskOutputs              , vertex:Vertex) : Seq[Vertex] = {
//    val outputVerticesAndChildren = taskOutputs.specs.flatMap({ spec:Spec =>
//
//      val outputVertex = new TaskOutputVertex(spec, taskOutputs.comments.value)
//      Edge.connect(vertex, outputVertex)
//
//      Seq(outputVertex)
//
//// TODO: Consider whether to allow the following:
////      val children = recursivelyProcessAST(spec, outputVertex)
////      Seq(outputVertex) ++ children
//    })
//
//    return outputVerticesAndChildren
    return justProcessChildren(taskOutputs, vertex)
  }

  private def recursivelyProcess( taskPackageNames        : TaskPackageNames         , vertex:Vertex) : Seq[Vertex] = {
    ???
  }

  private def recursivelyProcess( taskParams              : TaskParams               , vertex:Vertex) : Seq[Vertex] = {
//    val paramVerticesAndChildren = taskParams.specs.flatMap({ spec:Spec =>
//
//      val paramVertex = new TaskParamVertex(spec, taskParams.comments.value)
//      Edge.connect(paramVertex, vertex)
//
//      val children = recursivelyProcessAST(spec, paramVertex)
//
//      Seq(paramVertex) ++ children
//    })
//
//    return paramVerticesAndChildren
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

    vertex match {
      case RootVertex() => /* This space intentionally left blank. */
      case _            => throw new RuntimeException("Workflow definition should have had parent RootVertex, but did not")
    }

    val children = workflowDefinition.elements.flatMap({ element:ASTType => recursivelyProcessAST(element, vertex)})

    return children
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
    // even though we expect it to be empty.
    val children = processChildren(astNode, vertex)

    return current ++ children
  }

  private def processChildren(astNode:ASTType, vertex:Vertex) : Seq[Vertex] = {
    return astNode.children.flatMap({ child:ASTType => recursivelyProcessAST(child, vertex)})
  }

}

