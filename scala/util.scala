package ducttape.util

import scala.sys.process._
import scala.io._
import java.io._

object Files {
  def write(str: String, file: File) = {
    val fw = new FileWriter(file)
    fw.write(str)
    fw.close()    
  }
}

object Shell {
  def run(cmds: Seq[String]) {
    run(cmds.mkString("\n"))
  }

  def run(cmd: String) {
    def provideIn(x: OutputStream) = {
      val bash = new PrintStream(x)
      bash.println("set -eo pipefail")
      bash.println(cmd)
      bash.close()
    }
    def handleOut(x: InputStream) = { Source.fromInputStream(x).getLines().foreach( println(_) ) }
    def handleErr(x: InputStream) = { Source.fromInputStream(x).getLines().foreach( println(_) ) }
    var code = "bash".run(new ProcessIO(provideIn, handleOut, handleErr)).exitValue()
    println("Returned %s".format(code))
  }

  def runGetOutput(cmd: String): String = {
    def provideIn(x: OutputStream) = {
      val bash = new PrintStream(x)
      bash.println("set -eo pipefail")
      bash.println(cmd)
      bash.close()
    }
    val output = new StringBuilder
    def handleOut(x: InputStream) = { Source.fromInputStream(x).getLines().foreach( output.append(_) ) }
    def handleErr(x: InputStream) = { Source.fromInputStream(x).getLines().foreach( println(_) ) }
    var code = "bash".run(new ProcessIO(provideIn, handleOut, handleErr)).exitValue()
    println("Returned %s".format(code))
    output.toString
  }
}
