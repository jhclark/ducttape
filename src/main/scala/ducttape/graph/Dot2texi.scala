// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.graph

import ducttape.syntax.AbstractSyntaxTree._


object Dot2texi {

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
        case goal:   GoalVertex        => /* This space intentionally left blank. */
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
    else if (task.contents.keyword=="package" || task.contents.keyword=="of") s.append(""", gray, rounded corners""")
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
    vertices.collect({case task:SummaryOfVertex => task}).foreach({task:SummaryOfVertex => drawTaskSubgraphBox(task, s)})
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
    s.append("""\definecolor{antiquebrass}{rgb}{0.8, 0.58, 0.46}""").append('\n')
    s.append("""\definecolor{amaranth}{rgb}{0.9, 0.17, 0.31}""").append('\n')
    s.append('\n')
    s.append("""\pgfdeclarelayer{background}""").append('\n')
    s.append("""\pgfdeclarelayer{foreground}""").append('\n')
    s.append("""\pgfsetlayers{background,main,foreground}""").append('\n')
    s.append('\n')
    s.append("""\tikzstyle{BranchSpecVertex} = [ellipse, draw=none, inner sep=0.3mm, fill=blue, drop shadow, text centered, anchor=north, text=white]""").append('\n')
    s.append("""\tikzstyle{task}   = [rectangle, draw=none, rounded corners=2mm, fill=orange, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]""").append('\n')
    s.append("""\tikzstyle{PackageVertex}   = [rectangle, draw=none, rounded corners=2mm, fill=orange, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]""").append('\n')
    s.append("""\tikzstyle{ConfigDefinitionVertex}   = [rectangle, draw=none, rounded corners=2mm, fill=deepchampagne, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]""").append('\n')
    s.append("""\tikzstyle{SummaryVertex}   = [rectangle, draw=none, rounded corners=2mm, fill=antiquebrass, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]""").append('\n')
    s.append("""\tikzstyle{SummaryOfVertex}   = [rectangle, draw=none, rounded corners=2mm, fill=amaranth, drop shadow, text centered, anchor=north, text=white, inner sep=1mm]""").append('\n')
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
    s.append("""\tikzstyle{GoalVertex} = [rectangle, rounded corners, draw, inner sep=0.3mm, fill=darkgray, text centered, anchor=north, text=white]""").append('\n')
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
