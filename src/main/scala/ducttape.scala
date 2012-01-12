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
      err.println("Usage: ducctape workflow.tape [--purge] [--viz] [--env taskName [realName]] [--markDone taskName [realName]]")
      exit(1)
    }
    if(args.length == 0) usage()

    val mode: String = args.length match {
      case 1 => "execute"
      case _ => args(1) match {
        case "--list" => "list"
        case "--purge" => "purge"
        case "--viz" => "viz"
        case "--env" => "env"
        case "--markDone" => "markDone"
        case _ => usage(); ""
      }
    }

    // format exceptions as nice error messages
    def ex2err[T](func: => T): T = {
      import ducttape.syntax.FileFormatException
      try { func } catch {
        case e: FileFormatException => {
          err.println("%sERROR: %s%s".format(conf.errorColor, e.getMessage, conf.resetColor))
          for( (file: File, line: Int, col: Int, untilLine: Int) <- e.refs) {
            err.println("%s%s:%d%s".format(conf.errorLineColor, file.getAbsolutePath, line, conf.resetColor))
            val badLines = io.Source.fromFile(file).getLines.drop(line-1).take(line-untilLine+1)
            err.println(conf.errorScriptColor + badLines.mkString("\n"))
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

    // make these messages optional with verbosity levels?
    var file = new File(args(0))
    //println("Reading workflow from %s".format(file.getAbsolutePath))
    val wd: WorkflowDefinition = ex2err(GrammarParser.read(file))
    //println("Building workflow...")
    val workflow: HyperWorkflow = ex2err(WorkflowBuilder.build(wd))
    //println("Workflow contains %d tasks".format(workflow.dag.size))
    
    // TODO: Check that all input files exist

    val baseDir = file.getAbsoluteFile.getParentFile
    val dirs = new DirectoryArchitect(baseDir)

    mode match {
      case "list" => {
        for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
          val taskT: TaskTemplate = v.packed.value
          val task: RealTask = taskT.realize(v)
          println("%s %s".format(task.name, task.realizationName))
        }
      }
      case "purge" => {
        // TODO: Confirm this drastic action
        val visitor: PackedDagVisitor = new Purger(conf, dirs)
        for(v: PackedWorkVert <- workflow.packedWalker.iterator) {
          visitor.visit(v.value)
        }
      }
      case "env" => {
        // TODO: Apply filters so that we do much less work to get here
        // TODO: Complain if wrong number of args
        val goalTaskName = args(2)
        val goalRealName = args(3)
        for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
          val taskT: TaskTemplate = v.packed.value
          if(taskT.name == goalTaskName) {
            val task: RealTask = taskT.realize(v)
            if(task.realizationName == goalRealName) {
              val env = new TaskEnvironment(dirs, task)
              for( (k,v) <- env.env) {
                println("%s=%s".format(k,v))
              }
            }
          }
        }
      }
      case "markDone" => {
        // TODO: Apply filters so that we do much less work to get here
        val goalTaskName = args(2)
        for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
          val taskT: TaskTemplate = v.packed.value
          if(taskT.name == goalTaskName) {
            val task: RealTask = taskT.realize(v)
            val env = new TaskEnvironment(dirs, task)
            if(CompletionChecker.isComplete(env)) {
              err.println("Task already complete")
            } else {
              CompletionChecker.forceCompletion(env)
              err.println("Forced completion of task")
            }
          }
        }
      }
      case "execute" => {
        def visitAll(visitor: UnpackedDagVisitor) {
          for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
            val taskT: TaskTemplate = v.packed.value
            val task: RealTask = taskT.realize(v)
            visitor.visit(task)
          }
        }
        err.println("Removing partial output...")
        visitAll(new PartialOutputRemover(conf, dirs))
        err.println("Retreiving code and building...")
        visitAll(new Builder(conf, dirs))
        err.println("Executing tasks...")
        visitAll(new Executor(conf, dirs))
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
