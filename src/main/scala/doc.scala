import java.io._
import io._
import collection.JavaConversions._
import collection._
import System._

trait Formatter {
  def header()
  def section(name: String)
  def subsection(name: String)
  def comment(content: Seq[String])
  def code(content: Seq[String])
  def footer()
}

class Latex extends Formatter {
  def header() = {
    // annoying substitution of "\\" to avoid unicode escape
    println("""
            \documentclass[10pt]{article}
            """+"\\"+"""usepackage[verbose, letterpaper, noheadfoot, margin=0.75in]{geometry}

            \title{\textbf{DuctTape} by Example}
            \author{Jonathan Clark}
            \date{}

            \begin{document}
            \thispagestyle{empty}
            \maketitle

            \tableofcontents

            \section{Introduction}
            This is documentation for DuctTape.
            """.stripMargin)

  }
  def section(name: String) = println("""\section{%s}""".format(name))
  def subsection(name: String) = println("""\subsection{%s}""".format(name))
  def comment(content: Seq[String]) = {
    for(line <- content) {
      println(line + """\\""")
    }
  }
  def code(content: Seq[String]) = {
    println("""\makebox[\textwidth]{\hrulefill}""")
    println("""\begin{verbatim}""")
    println(content.mkString("\n"))
    println("""\end{verbatim}""")
    println("""\makebox[\textwidth]{\hrulefill}""")
  }
  def footer() = println("""\end{document}""")
}

class Markdown extends Formatter {
  def header() = {
    println("""DuctTape by Example

By Jonathan Clark

All examples in this tutorial are runnable (and in fact used as regression tests).
You can find them in ./syntax/tutorial/

""")
  }
  def section(name: String) = {
    println(name)
    println("=" * name.length)
    println()
  }
  def subsection(name: String) = {
    println(name)
    println("-" * name.length)
    println()
  }
  def comment(content: Seq[String]) = {
    println(content.mkString("\n"))
    println()
  }
  def code(content: Seq[String]) = {
    println("```")
    for(line <- content) {
      println(line)
    }
    println("```")
    println()
  }
  def footer() = {}
}

object DuctTapeDoc {
  def main(args: Array[String]) {
    if(args.size != 2) {
      err.println("Usage: DuctTapeDoc ./syntax/tutorial/ [--latex|--markdown]")
      exit(1)
    }
    val docRoot = args(0)

    implicit val f: Formatter = args(1) match {
      case "--latex" => new Latex
      case "--markdown" => new Markdown
    }

    f.header()

    for(dir <- new File(docRoot).listFiles.toList.sort((e1,e2) => (e1.getName < e2.getName))) {
      if(dir.isDirectory) {
        val docFiles = dir.listFiles.toList.filter(_.getName.endsWith(".tape")).sort((e1,e2) => (e1.getName < e2.getName))
        if(docFiles.size > 0) {
          f.section(dir.getName)
          for(file <- docFiles) {
            handleFile(file)
          }
        }
      }
    }
    f.footer()
  }

  def handleFile(file: File)(implicit f: Formatter) = {
    val buffer = new mutable.ArrayBuffer[String]
    var kind = ""
    def dump() = {
      if(buffer.size > 0) {
        kind match {
          case "comment" => f.comment(buffer)
          case "code" => {
            val lines = buffer.filter(_.trim().length > 0)
            if(lines.size > 0)
              f.code(lines)
          }
        }
        buffer.clear
      }
    }
    def append(line: String, knd: String) = {
      if(knd != kind)
        dump()
      kind = knd
      buffer += line
    }
    val COMMENT_PAT = """^\s*#(.*)$""".r
    val lines = Source.fromFile(file).getLines.toSeq
    val title = lines.head.substring(1) // first line is always title
    f.subsection("%s (%s)".format(title, file.getName))
    for(line <- lines.drop(1)) line match {
      case COMMENT_PAT(content) => append(content, "comment")
      case _ => append(line, "code")
    }
    dump()
  }
}
