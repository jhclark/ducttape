import System._
import collection._

import java.io.File

import ducttape._
import ducttape.environment._
import ducttape.hyperdag._
import ducttape.Types._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.GrammarParser
import ducttape.workflow._
import ducttape.util._
import ducttape.versioner._

package ducttape {
  class Config {
    var headerColor = Console.BLUE
    var byColor = Console.BLUE
    var taskColor = Console.GREEN
    var errorColor = Console.RED
    var resetColor = Console.RESET

    var modeColor = Console.GREEN

    var errorLineColor = Console.BLUE // file and line number of error
    var errorScriptColor = Console.WHITE // quote from file

    var taskNameColor = Console.GREEN
    var realNameColor = Console.BLUE
  }
}

object Ducttape {

  import com.frugalmechanic.optparse._
  class Mode(val name: String, val desc: String) extends OptParse {
    def optCount = allOpts.size
    def unapply(name: String) = if(name == this.name) Some(name) else None
  }

  class Opts(conf: Config, args: Array[String]) extends OptParse {
    // TODO: Do some reflection magic on modes to enable automatic subtask names
    val exec = new Mode("exec", desc="Execute the workflow (default if no mode is specified)") {
      val jobs = IntOpt(desc="Number of concurrent jobs to run")
    }
    val list = new Mode("list", desc="List the tasks and realizations defined in the workflow");
    val env = new Mode("env", desc="Show the environment variables that will be used for a task/realization");
    val viz = new Mode("viz", desc="Output a GraphViz dot visualization of the unpacked workflow");
    val debug_viz = new Mode("debug_viz", desc="Output a GraphViz dot visualization of the packed MetaHyperDAG");
    val mark_done = new Mode("mark_done", desc="Mark a specific task/realization as complete (useful if some manual recovery or resumption was necessary)");
    val invalidate = new Mode("invalidate", desc="Mark a specific task/realization and all of its children as complete");

    val modes = Seq(exec, list, env, viz, debug_viz, mark_done, invalidate)

    // Positional arguments:
    private var _workflowFile = new File(".")
    def workflowFile = _workflowFile

    private var _mode = "exec"
    def mode = _mode

    private var _taskName: Option[String] = None
    def taskName: Option[String] = _taskName

    private var _realNames: Seq[String] = Nil
    def realNames: Seq[String] = _realNames

    // TODO: Can we define help as an option?
    override def help {
      err.println("Usage: ducttape workflow.tape [mode] [taskName [realizationNames...]] [--options]")
      err.println("Available modes: %s (default) %s".format(modes.head.name, modes.drop(1).map(_.name).mkString(" ")))
      for(mode <- modes) {
        // TODO: Change visibility of init to protected instead of this hack...
        mode.parse(Array())

        if(mode.optCount > 1) {
          err.println("%s%s mode:%s".format(conf.modeColor, mode.name, conf.resetColor))
          mode.help
        }
      }
    }

    def exitHelp(msg: String, code: Int) {
      help
      exit(msg, code)
    }

    val leftoversOpt = defaultOpt(MultiStrOpt())
    parse(args)
    val posArgs = leftoversOpt.getOrElse(Nil)
    if(posArgs.size == 0) {
      exitHelp("Workflow file is required", 1)
    }
    // TODO: More general positional args parsing
    if(posArgs.size >= 1)
      _workflowFile = new File(posArgs(0))
    if(posArgs.size >= 2)
      _mode = posArgs(1)
    if(posArgs.size >= 3)
      _taskName = Some(posArgs(2))
    if(posArgs.size >= 4)
      _realNames = posArgs.drop(3)
  }

  def main(args: Array[String]) {
    val conf = new Config
    err.println("%sDuctTape v0.1".format(conf.headerColor))
    err.println("%sBy Jonathan Clark".format(conf.byColor))
    err.println(Console.RESET)

    val opts = new Opts(conf, args)

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
/*
        case e: Exception => {
          err.println("%sERROR: %s".format(conf.errorColor, e.getMessage))
          exit(1)
          throw new Error("Unreachable") // make the compiler happy
        }
*/
        case t: Throwable => throw t
      }
    }

    // make these messages optional with verbosity levels?
    //println("Reading workflow from %s".format(file.getAbsolutePath))
    val wd: WorkflowDefinition = ex2err(GrammarParser.read(opts.workflowFile))
    //println("Building workflow...")
    val workflow: HyperWorkflow = ex2err(WorkflowBuilder.build(wd))
    //println("Workflow contains %d tasks".format(workflow.dag.size))
    
    // TODO: Check that all input files exist

    val baseDir = opts.workflowFile.getAbsoluteFile.getParentFile
    val dirs = new DirectoryArchitect(baseDir)

    def colorizeDirs(list: Traversable[(String,String)]): Seq[String] = {
      list.toSeq.map{case (name, real) => {
        "%s/%s%s%s/%s%s%s".format(baseDir.getAbsolutePath,
                                  conf.taskNameColor, name, conf.resetColor,
                                  conf.realNameColor, real, conf.resetColor)
      }}
    }

    def visitAll(visitor: UnpackedDagVisitor, versions: WorkflowVersioner, numCores: Int = 1) {
      workflow.unpackedWalker.foreach(numCores, { v: UnpackedWorkVert => {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v, versions)
        visitor.visit(task)
      }})
    }

    err.println("Checking for completed steps...")
    val (cc: CompletionChecker, versions: ExecutionVersioner) = {
      val initVersioner = new MostRecentWorkflowVersioner(dirs)
      val cc = new CompletionChecker(conf, dirs, initVersioner)
      visitAll(cc, initVersioner)
      (cc, new ExecutionVersioner(cc.completedVersions, initVersioner.nextVersion))
    }

    def list {
      for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v, versions)
        println("%s %s".format(task.name, task.realizationName))
        //println("Actual realization: " + v.realization)
      }
    }
    def env {
      if(opts.taskName == None) {
        err.println("ERROR: env requires a taskName")
      }
      if(opts.realNames.size != 1) {
        err.println("ERROR: env requires one realization name")
      }
      val goalTaskName = opts.taskName.get
      val goalRealName = opts.realNames.head

      // TODO: Apply filters so that we do much less work to get here
      for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
        val taskT: TaskTemplate = v.packed.value
        if(taskT.name == goalTaskName) {
          val task: RealTask = taskT.realize(v, versions)
          if(task.realizationName == goalRealName) {
            val env = new TaskEnvironment(dirs, versions, task)

            for( (k,v) <- env.env) {
              println("%s=%s".format(k,v))
            }
          }
        }
      }
    }

    def markDone {
      if(opts.taskName == None) {
        err.println("ERROR: mark_done requires a taskName")
      }
      if(opts.realNames.size < 1) {
        err.println("ERROR: mark_done requires realization names")
      }
      val goalTaskName = opts.taskName.get
      val goalRealNames = opts.realNames.toSet

      // TODO: Apply filters so that we do much less work to get here
      for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
        val taskT: TaskTemplate = v.packed.value
        if(taskT.name == goalTaskName) {
          val task: RealTask = taskT.realize(v, versions)
          if(goalRealNames(task.realizationName)) {
            val env = new TaskEnvironment(dirs, versions, task)
            if(CompletionChecker.isComplete(env)) {
              err.println("Task already complete: " + task.name + "/" + task.realizationName)
            } else {
              CompletionChecker.forceCompletion(env)
              err.println("Forced completion of task: " + task.name + "/" + task.realizationName)
            }
          }
        }
      }
    }

    def exec {
      err.println("About to run the following tasks:")
      err.println(colorizeDirs(cc.todo).mkString("\n"))
      
      if(cc.partial.size > 0) {
        err.println("About to permenantly delete the partial output in the following directories:")
        err.println(colorizeDirs(cc.partial).mkString("\n"))
        err.print("Are you sure you want to DELETE all these? [y/n] ") // user must still press enter
      } else {
        err.print("Are you sure you want to run all these? [y/n] ") // user must still press enter
      }
      
      Console.readChar match {
        case 'y' | 'Y' => {
          err.println("Removing partial output...")
          throw new Error("TODO: Be careful about removing output")
          visitAll(new PartialOutputRemover(conf, dirs, versions, cc.partial), versions)
          err.println("Retreiving code and building...")
          visitAll(new Builder(conf, dirs, versions, cc.todo), versions)
          err.println("Executing tasks...")
          visitAll(new Executor(conf, dirs, versions, workflow, cc.completed, cc.todo), versions, opts.exec.jobs())
        }
        case _ => err.println("Doing nothing")
      }
    }

    def viz {
      err.println("Generating GraphViz dot visualization...")
      import ducttape.viz._
      println(GraphViz.compileXDot(WorkflowViz.toGraphViz(workflow, versions)))
    }

    def debugViz {
      err.println("Generating GraphViz dot visualization of MetaHyperDAG...")
      import ducttape.viz._
      println(workflow.dag.toGraphVizDebug)
    }

    def invalidate {
      if(opts.taskName == None) {
        err.println("ERROR: invalidate requires a taskName")
      }
      if(opts.realNames.size < 1) {
        err.println("ERROR: invalidate requires realization names")
      }
      val taskToKill = opts.taskName.get
      val realsToKill = opts.realNames.toSet
      err.println("Invalidating task %s for realizations: %s".format(taskToKill, realsToKill))

      throw new Error("Be more careful about how invalidation happens")
      
      // 1) Accumulate the set of changes
      // we'll have to keep a map of parents and use
      // the dag to keep state
      val victims = new mutable.HashSet[(String,String)]
      val victimList = new mutable.ListBuffer[(String,String)]
      for(v: UnpackedWorkVert <- workflow.unpackedWalker.iterator) {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v, versions)
        if(taskT.name == taskToKill) {
          if(realsToKill(task.realizationName)) {
            //err.println("Found victim %s/%s".format(taskT.name, task.realizationName))
            // TODO: Store seqs instead?
            victims += ((task.name, task.realizationName))
            victimList += ((task.name, task.realizationName))
          }
        } else {
          // was this task invalidated by its parent?
          // TODO: Can we propagate this in a more natural way
          val isVictim = task.inputVals.exists{ case (_, _, srcTaskDef, srcRealization) => {
            val parent = (srcTaskDef.name, Task.realizationName(Task.branchesToMap(srcRealization)))
              victims(parent)
          }}
          if(isVictim) {
            //err.println("Found indirect victim %s/%s".format(task.name, task.realizationName))
            victims += ((task.name, task.realizationName))
            victimList += ((task.name, task.realizationName))
          }
        }
      }
      
      // 2) prompt the user
      // TODO: Increment version instead of deleting?
      err.println("About to permenantly delete the following directories:")
      // TODO: Use directory architect here
      val absDirs = victimList.map{case (name, real) => new File(baseDir, "%s/%s".format(name,real))}
      err.println(colorizeDirs(victimList).mkString("\n"))
      
      err.print("Are you sure you want to delete all these? [y/n] ") // user must still press enter
      Console.readChar match {
        case 'y' | 'Y' => absDirs.foreach(f => { err.println("Deleting %s".format(f.getAbsolutePath)); Files.deleteDir(f) })
        case _ => err.println("Doing nothing")
      }
    }

    // TODO: Have run() function in each mode?
    opts.mode match {
      case "list" => list
      case "env" => env
      case "mark_done" => markDone
      case "viz" => viz
      case "debug_viz" => debugViz
      case "invalidate" => invalidate
      case _ => exec
    }
  }
}
