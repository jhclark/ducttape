package ducttape.viz

import sys.process._
import java.io._
import ducttape.util._

object GraphViz {

  def escape(str: String) = str.replace("\"", "\\\"").replace("\n", "\\n")
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