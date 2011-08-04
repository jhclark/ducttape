import java.io._
import io._
import collection.JavaConversions._
import System._

object DuctTapeDoc {
  def main(args: Array[String]) {
    if(args.size != 1) {
      err.println("Usage: DuctTapeDoc ./syntax/tutorial/")
      exit(1)
    }
    val docRoot = args(0)    

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

    for(dir <- new File(docRoot).listFiles.toList.sort((e1,e2) => (e1.getName < e2.getName))) {
      if(dir.isDirectory) {
        println("""\section{%s}""".format(dir.getName))
        val docFiles = dir.listFiles.toList.filter(_.getName.endsWith(".tape")).sort((e1,e2) => (e1.getName < e2.getName))
        for(file <- docFiles) {
          println("""\subsection{%s}""".format(file.getName))
          println("""\makebox[\textwidth]{\hrulefill}""")
          println("""\begin{verbatim}""")
          println(Source.fromFile(file).mkString)
          println("""\end{verbatim}""")
          println("""\makebox[\textwidth]{\hrulefill}""")
        }
      }
    }

    println("""\end{document}""")
  }
}
