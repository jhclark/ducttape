package ducttape.io

import scala.sys.process._
import scala.io._
import java.io._

class MakeReader {
  def shell(cmd: String) = {
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

  def parse(file: String) = {
    println("Reading %s...".format(file))
    
    val CommentRE = """\s*#.*""".r
  // TODO: Make : optional
    val HeadRE = """\[([a-zA-Z0-9.-]+)\]\s*([a-zA-Z0-9.-]+)\s*:\s*([a-zA-Z0-9.-]+)""".r
    val CmdRE = """\s+(.+)""".r

    var nLine = 0
    Source.fromFile(file, "utf-8").getLines.foreach(line => {
      nLine += 1
      line match {
	case CommentRE => {
	  println("comment %s".format(line))
	}
	case HeadRE(step, consequent, antecedent) => {
	  println("header %s %s %s".format(step, consequent, antecedent))
	}
	case CmdRE(cmd) => {
	  println("command %s".format(cmd))
	  shell(cmd)
	}
	case entry => {
	  println("Unrecognized line at %s:%s %s".format(file, nLine, entry))
	}
      }
    })
  }
}

// main
//def file = args(0)
//vparse(file)
