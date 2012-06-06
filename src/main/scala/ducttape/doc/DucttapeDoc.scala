package ducttape.doc

import java.io._
import io._
import collection.JavaConversions._
import collection._
import System._
import ducttape.util.Files
import ducttape.syntax.GrammarParser
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.syntax.AbstractSyntaxTree.TaskDef

trait Formatter {
  def header()
  def section(name: String)
  def subsection(name: String)
  def comment(content: Seq[String])
  def code(content: Seq[String])
  def footer()
}

object Latex {
  def escape(str: String) = str.
    replace("\\","\\\\").
    replace("_","-").
    replace("#","\\#").
    replace("$","\\$").
    replace("&", "\\&")
}

class Latex(headerFile: File) extends Formatter {
  def header() = {
    // annoying substitution of "\\" to avoid unicode escape
    println(Files.read(headerFile).mkString("\n"))

  }

  def section(name: String) = println("""\section{%s}""".format(name))

  def subsection(name: String) = println("""\subsection{%s}""".format(name))

  def comment(content: Seq[String]) = {
    for (line <- content) {
      val escaped = Latex.escape(line).trim()
      if (!escaped.isEmpty)
        println(escaped + """\\""")
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

class Markdown(headerFile: File) extends Formatter {
  def header() = {
    println(Files.read(headerFile).mkString("\n"))
    println()
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
    for (line <- content) {
      println(line)
    }
    println("```")
    println()
  }
  def footer() = {}
}

object DucttapeDoc {
  def main(args: Array[String]) {
    if(args.size != 2) {
      err.println("Usage: DuctTapeDoc ./tutorial/ [--latex|--markdown]")
      exit(1)
    }
    val docRoot = new File(args(0))

    implicit val f: Formatter = args(1) match {
      case "--latex" => new Latex(new File(docRoot, "header.tex"))
      case "--markdown" => new Markdown(new File(docRoot, "header.md"))
    }

    f.header()

    val tapeAndMarkdownFiles = Files.ls(docRoot).
      filter { f => f.getName.endsWith(".tape") ||
               (f.getName.endsWith(".md") && f.getName != "TUTORIAL.md" && f.getName != "header.md") }.
      sortWith { (e1,e2) => e1.getName < e2.getName }
      
    System.err.println("Found %d files in : %s".format(tapeAndMarkdownFiles.size, docRoot.getAbsolutePath))
      
    var prevSectionName = ""
    for (file <- tapeAndMarkdownFiles) {
      
      if (file.getName.endsWith(".tape")) {
        
        //System.err.println("Analyzing file: " + file.getAbsolutePath)
        
        val workflow: WorkflowDefinition = GrammarParser.readWorkflow(file)
        val headerComments: Seq[String] = workflow.blocks.headOption match {
          case None => Seq.empty
          case Some(block) => block.comments.value match {
            case None => Seq.empty
            case Some(comments) => comments.split('\n').toSeq.zipWithIndex.filter {
              case (line, i) if (i == 0) => !line.startsWith("!") // weed out shebangs
              case _ => true
            }.map(_._1)
          }
        }
        
        val sectionName: String = headerComments.find(_.contains("Chapter ")) match {
          case Some(line) => line.split(":")(1).trim 
          case None => throw new RuntimeException("No chapter name in " + file.getAbsolutePath)
        }
        val lessonName: String = headerComments.find(_.contains("Lesson ")) match {
          case Some(line) => line.split(":")(1).trim 
          case None => throw new RuntimeException("No lesson name in " + file.getAbsolutePath)
        }
        
        if (sectionName != prevSectionName) {
          f.section(sectionName)
          prevSectionName = sectionName
        }
        System.err.println("Processing %s: %s".format(sectionName, lessonName))
        handleFile(sectionName, lessonName, workflow, file)
      } else {
        System.err.println("Ignoring markdown file: " + file.getAbsolutePath)
      }
    }
    f.footer()
  }

  def handleFile(
      sectionName: String,
      lessonName: String,
      workflow: WorkflowDefinition,
      workflowFile: File)
     (implicit f: Formatter) = {
    
    val buffer = new mutable.ArrayBuffer[String]
    def dump(kind: String) = {
      if (buffer.size > 0) {
        kind match {
          case "comment" => f.comment(buffer)
          case "code" => {
            val lines = buffer.filter(_.trim().length > 0)
            if (lines.size > 0)
              f.code(lines)
          }
        }
        buffer.clear
      }
    }
    
    var curKind = "comment"
    def append(line: String, lineKind: String) = {
      if (lineKind != curKind)
        dump(curKind)
      buffer += line
      curKind = lineKind
    }
    
    f.subsection("%s: %s".format(sectionName, lessonName))
    
//    for (block <- workflow.blocks) {
//      block.comments
//    }
    val SHEBANG_PAT = """^#!.*""".r
    val CHAPTER_PAT = """^#\s*Chapter\s+[0-9]+:.*""".r
    val LESSON_PAT = """^#\s*Lesson\s+[0-9]+:.*""".r
    val COMMENT_PAT = """^\s*#(.*)$""".r
    for (line <- Files.read(workflowFile)) line match {
      case SHEBANG_PAT() => ;
      case CHAPTER_PAT() => ;
      case LESSON_PAT() => ;
      case COMMENT_PAT(content) => append(content, "comment")
      case _ => append(line, "code")
    }
    dump(curKind)
  }
}
