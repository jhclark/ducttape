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
  import ducttape.workflow._

  def toGraphViz(workflow: HyperWorkflow,
                 completed: Set[(String,String)] = Set.empty,
                 running: Set[(String,String)] = Set.empty,
                 failed: Set[(String,String)] = Set.empty) = {
    val str = new StringBuilder(1000)

    str ++= "digraph G {\n"

    def getName(t: String, r: String) = GraphViz.escape("%s/%s".format(t, r))

    // first, list vertices
    for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
      val taskT: TaskTemplate = v.packed.value
      val task: RealTask = taskT.realize(v)
      val color = (task.name, task.realizationName) match {
        case t if completed(t) => "dodgerblue1"
        case t if running(t) => "darkolivegreen4"
        case t if failed(t) => "firebrick"
        case _ => "white"
      }
      str ++= "\"%s\" [fillcolor=%s];\n".format(getName(task.name, task.realizationName), color)
    }

    // now list edges
    for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
      val taskT: TaskTemplate = v.packed.value
      val task: RealTask = taskT.realize(v)
      val child = getName(task.name, task.realizationName)
      task.inputVals.map{ case (_, _, srcTaskDef, srcRealization) => {
        getName(srcTaskDef.name, Task.realizationName(Task.branchesToMap(srcRealization)))
      }}.toSet.foreach{parent: String => {
        if(parent != child)
          str ++= "\"%s\" -> \"%s\";\n".format(parent, child) // TODO: Quote?
      }}
    }

    str ++= "}\n"
    str.toString
  }
}
