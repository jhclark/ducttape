// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
                                   stdPrefix="compile_xdot",
                                   workDir=new File("."),
                                   env=Seq.empty,
                                   stdin=Seq(dot)).mkString("\n")
  }
}
