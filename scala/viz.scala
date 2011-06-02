package ducttape.viz

import ducttape.util._

object GraphViz {
  def escape(str: String) = str.replace(' ', '_')
  def compile(str: String, outFile: String) = {
    import sys.process._
    import java.io._
    val temp = File.createTempFile("ducttape",".dot")
    Files.write(str, temp)
    "/usr/bin/dot -Tpdf" #< temp #> new File(outFile) ! ;
  }
}
