package ducttape.viz

import collection._

object GraphViz {
  import sys.process._
  import java.io._
  import ducttape.util._

  def escape(str: String) = str.replace(' ', '_').replace("\"", "\\\"")
  def compile(str: String, outFile: String) = {
    val temp = File.createTempFile("ducttape",".dot")
    Files.write(str, temp)
    compileFile(temp, outFile)
  }
  def compileFile(file: File, outFile: String) = {
    // TODO: Check for dot existing
    "dot -Tpdf" #< file #> new File(outFile) ! ;
  }
  def compileXDot(dot: String): String = {
    // TODO: Check for dot existing
    Shell.runGetOutputLinesNoShell(cmd="dot -Txdot",
                                   workDir=new File("."),
                                   env=Seq.empty,
                                   stdin=Seq(dot)).mkString("\n")
  }
}

object WorkflowViz {
  import ducttape.Types._
  import ducttape.versioner._
  import ducttape.workflow._

  def toGraphViz(workflow: HyperWorkflow,
                 versions: WorkflowVersioner,
                 completed: Set[(String,Realization)] = Set.empty,
                 running: Set[(String,Realization)] = Set.empty,
                 failed: Set[(String,Realization)] = Set.empty) = {

    val str = new StringBuilder(1000)
    str ++= "digraph G {\n"

    def getName(t: String, r: Realization) = GraphViz.escape("%s/%s".format(t, r.toString))

    // first, list vertices
    for(v: UnpackedWorkVert <- workflow.unpackedWalker().iterator) {
      val taskT: TaskTemplate = v.packed.value
      val task: RealTask = taskT.realize(v, versions)
      val color = (task.name, task.realization) match {
        case t if completed(t) => "dodgerblue1"
        case t if running(t) => "darkolivegreen4"
        case t if failed(t) => "firebrick"
        case _ => "white"
      }
      str ++= "\"%s\" [fillcolor=%s,style=filled];\n".format(getName(task.name, task.realization), color)
    }

    // now list edges
    for(v: UnpackedWorkVert <- workflow.unpackedWalker().iterator) {
      val taskT: TaskTemplate = v.packed.value
      val task: RealTask = taskT.realize(v, versions)
      val child = getName(task.name, task.realization)
      task.inputVals.map{ case (_, _, srcTaskDef, srcRealization) => {
        val srcReal = new Realization(srcRealization) // TODO: Hacky
        getName(srcTaskDef.name, srcReal)
      }}.toSet.foreach{parent: String => {
        if(parent != child)
          str ++= "\"%s\" -> \"%s\";\n".format(parent, child) // TODO: Quote?
      }}
    }

    str ++= "}\n"
    str.toString
  }
}
