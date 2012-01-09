import System._
import collection._

import java.io.File

import ducttape._
import ducttape.hyperdag._
import ducttape.Types._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.GrammarParser
import ducttape.workflow._
import ducttape.util._

package ducttape {
  class Config {
    var headerColor = Console.BLUE
    var byColor = Console.BLUE
    var taskColor = Console.GREEN
    var errorColor = Console.RED
    var resetColor = Console.RESET

    var errorLineColor = Console.BLUE // file and line number of error
    var errorScriptColor = Console.WHITE // quote from file
  }
}

object Ducttape {

  def main(args: Array[String]) {
    val conf = new Config

    println("%sDuctTape v0.1".format(conf.headerColor))
    println("%sBy Jonathan Clark".format(conf.byColor))
    println(Console.RESET)

    def usage() = {
      err.println("Usage: ducctape workflow.tape [--purge] [--viz]")
      exit(1)
    }
    if(args.length == 0) usage()

    // TODO: Confirm this drastic action (purge)
    val purge = {args.length > 1 && args(1) == "--purge"}
    val viz = {args.length > 1 && args(1) == "--viz"}

    val mode: String = args.length match {
      case 1 => "execute"
      case _ => args(1) match {
        case "--purge" => "purge"
        case "--viz" => "viz"
        case _ => usage(); ""
      }
    }

    // format exceptions as nice error messages
    def ex2err[T](func: => T): T = {
      import ducttape.syntax.FileFormatException
      try { func } catch {
        case e: FileFormatException => {
          err.println("%sERROR: %s%s".format(conf.errorColor, e.getMessage, conf.resetColor))
          for( (file: File, line: Int, col: Int) <- e.refs) {
            var badLine = io.Source.fromFile(file).getLines.drop(line-1).next
            err.println("%s%s:%d%s".format(conf.errorLineColor, file.getAbsolutePath, line, conf.resetColor))
            err.println(conf.errorScriptColor + badLine)
            err.println(" " * (col-2) + "^")
          }
          exit(1)
          throw new Error("Unreachable") // make the compiler happy
        }
        case e: Exception => {
          err.println("%sERROR: %s".format(conf.errorColor, e.getMessage))
          exit(1)
          throw new Error("Unreachable") // make the compiler happy
        }
        case t: Throwable => throw t
      }
    }

    var file = new File(args(0))
    println("Reading workflow from %s".format(file.getAbsolutePath))
    val wd: WorkflowDefinition = ex2err(GrammarParser.read(file))
    println("Building workflow...")
    val workflow: HyperWorkflow = ex2err(WorkflowBuilder.build(wd))
    println("Workflow contains %d tasks".format(workflow.dag.size))
    
    // TODO: Check that all input files exist

    println("Executing...")

    val baseDir = file.getAbsoluteFile.getParentFile
    val dirs = new DirectoryArchitect(baseDir)

    mode match {
      case "purge" => {
        val visitor: PackedDagVisitor = new Purger(conf, dirs)
        for(v: PackedWorkVert <- workflow.dag.packedWalker.iterator) {
          visitor.visit(v.value)
        }
      }
      case "execute" => {
        val visitor: UnpackedDagVisitor = new Executor(conf, dirs)
        for(v: UnpackedWorkVert <- workflow.dag.unpackedWalker.iterator) {
          val taskT: TaskTemplate = v.packed.value
          val task: RealTask = taskT.realize(v.realization)
          visitor.visit(task)
        }
      }
      case "viz" => {
        println("Compiling GraphViz visualization to viz.pdf...")
        import ducttape.viz._
        val dotFile = new File("viz.dot")
        Files.write(workflow.dag.toGraphViz, dotFile)
        GraphViz.compileFile(dotFile, "viz.pdf")
      }
    }
  }
}
