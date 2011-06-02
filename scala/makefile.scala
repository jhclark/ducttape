package ducttape.io

import scala.io._

import ducttape.util._

class MakeReader {
  def parse(file: String) = {
    println("Reading %s...".format(file))
    
    val CommentRE = """\s*#.*""".r
  // TODO: Make : optional
    val HeadRE = """\[([a-zA-Z0-9.-]+)\]\s*([a-zA-Z0-9.-]+)\s*(?::\s*([a-zA-Z0-9.-]+))""".r
    val CmdRE = """\s+(.+)""".r
    val BlankRE = """^\s*$""".r

    var nLine = 0
    Source.fromFile(file, "utf-8").getLines.foreach(line => {
      nLine += 1
      line match {
        case BlankRE => {
          //comments.clear
        }
	case CommentRE => {
	  println("comment %s".format(line))
	}
	case HeadRE(step, consequent, antecedent) => {
	  println("header %s %s %s".format(step, consequent, antecedent))
	}
	case CmdRE(cmd) => {
	  println("command %s".format(cmd))
	  Shell.run(cmd)
	}
	case entry => {
	  println("Unrecognized line at %s:%s %s".format(file, nLine, entry))
	}
      }
    })
  }
}

object MakeReader {
  def main(args: Array[String]) = {
    def file = args(0)
    val reader = new MakeReader
    reader.parse(file)
  }
}

