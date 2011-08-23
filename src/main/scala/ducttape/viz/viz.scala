package ducttape.viz

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
    "/usr/bin/dot -Tpdf" #< file #> new File(outFile) ! ;
  }
}
