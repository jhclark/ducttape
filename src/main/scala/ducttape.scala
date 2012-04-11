import System._
import collection._
import java.io.File
import java.util.concurrent.ExecutionException
import ducttape._
import ducttape.exec.CompletionChecker
import ducttape.exec.Executor
import ducttape.exec.InputChecker
import ducttape.exec.PackageBuilder
import ducttape.exec.PackageFinder
import ducttape.exec.TaskEnvironment
import ducttape.exec.UnpackedDagVisitor
import ducttape.exec.DirectoryArchitect
import ducttape.exec.PackageVersioner
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.GrammarParser
import ducttape.syntax.StaticChecker
import ducttape.syntax.ErrorBehavior._
import ducttape.versioner._
import ducttape.workflow.WorkflowBuilder
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.Realization
import ducttape.workflow.TaskTemplate
import ducttape.workflow.RealTask
import ducttape.workflow.BranchPoint
import ducttape.workflow.Branch
import ducttape.workflow.RealizationPlan
import ducttape.workflow.Types._
import ducttape.util.Files
import ducttape.util.OrderedSet
import ducttape.util.MutableOrderedSet
import ducttape.exec.FullTaskEnvironment

package ducttape {
  class Config {
    // TODO: Use Map for color sot that we can remove all of them easily?
    var headerColor = Console.BLUE
    var byColor = Console.BLUE
    var taskColor = Console.CYAN
    var warnColor = Console.BOLD + Console.YELLOW
    var errorColor = Console.RED
    var resetColor = Console.RESET

    var modeColor = Console.GREEN

    var errorLineColor = Console.BLUE // file and line number of error
    var errorScriptColor = Console.WHITE // quote from file

    var taskNameColor = Console.CYAN
    var realNameColor = Console.BLUE

    var greenColor = Console.GREEN
    var redColor = Console.RED
    
    // TODO: Enum?
    def clearColors() {
      headerColor = ""
      byColor = ""
      taskColor = ""
      warnColor = ""
      errorColor = ""
      resetColor = ""
  
      modeColor = ""
  
      errorLineColor = ""
      errorScriptColor = ""
  
      taskNameColor = ""
      realNameColor = ""
  
      greenColor = ""
      redColor = ""
    }
  }
  
  object ProcessInfo {
    import java.lang.management._
    // vmName looks like 11411@mymachine.org
    lazy val vmName = ManagementFactory.getRuntimeMXBean.getName
    lazy val pid = vmName.split("@")(0).toInt
    lazy val hostname = vmName.split("@")(1)
  }
}

object Ducttape {

  import com.frugalmechanic.optparse._
  class Mode(val name: String, val desc: String) extends OptParse {
    def optCount = allOpts.size
    def unapply(name: String) = if(name == this.name) Some(name) else None
  }

  class Opts(conf: Config, args: Seq[String]) extends OptParse {
    //override val optParseDebug = true

    // TODO: Do some reflection and object apply() magic on modes to enable automatic subtask names
    val exec = new Mode("exec", desc="Execute the workflow (default if no mode is specified)") {
    }
    val jobs = IntOpt(desc="Number of concurrent jobs to run", default=1)
    val config_file = StrOpt(desc="Stand-off workflow configuration file to read", short='C')
    val config_name = StrOpt(desc="Workflow configuration name to run", short='c', invalidWith=config_file)
    val yes = BoolOpt(desc="Don't prompt or confirm actions. Assume the answer is 'yes' and just do it.")
    val no_color = BoolOpt(desc="Don't colorize output")
    
    val list = new Mode("list", desc="List the tasks and realizations defined in the workflow");
    val env = new Mode("env", desc="Show the environment variables that will be used for a task/realization");
    val viz = new Mode("viz", desc="Output a GraphViz dot visualization of the unpacked workflow");
    val debug_viz = new Mode("debug_viz", desc="Output a GraphViz dot visualization of the packed MetaHyperDAG");
    val mark_done = new Mode("mark_done", desc="Mark a specific task/realization as complete (useful if some manual recovery or resumption was necessary)");
    val invalidate = new Mode("invalidate", desc="Mark a specific task/realization and all of its children as invalid -- they won't be deleted, but they will be re-run with the latest version of your code and data");
    val purge = new Mode("purge", desc="Permenantly delete a specific task/realization and all of its children (recommend purging instead)");

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
    // TODO: Rewrite arg parsing as a custom module?
    override def help {
      err.println("Usage: ducttape workflow.tape [--options] [mode [taskName [realizationNames...]]]")
      err.println("Available modes: %s (default) %s".format(modes.head.name, modes.drop(1).map(_.name).mkString(" ")))
      super.help

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
      err.println("%sERROR: %s%s".format(conf.errorColor, msg, conf.resetColor))
      System.exit(code)
    }

    if(args.isEmpty || args(0).startsWith("-")) {
      exitHelp("Workflow file is required", 1)
    }

    _workflowFile = new File(args(0))

    private val leftoversOpt = defaultOpt(MultiStrOpt())
    parse(args.drop(1).toArray) // skip workflow file
    private val posArgs = leftoversOpt.getOrElse(Nil)
    // TODO: More general positional args parsing
    if(posArgs.size >= 1)
      _mode = posArgs(0)
    if(posArgs.size >= 2)
      _taskName = Some(posArgs(1))
    if(posArgs.size >= 3)
      _realNames = posArgs.drop(2)
  }

  def main(args: Array[String]) {
    val conf = new Config
    val opts = new Opts(conf, args)
    if(opts.no_color) {
      conf.clearColors()
    }
    
    err.println("%sDuctTape v0.1".format(conf.headerColor))
    err.println("%sBy Jonathan Clark".format(conf.byColor))
    err.println(Console.RESET)

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
          sys.exit(1)
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
    //println("Reading workflow from %s".format(file.getAbsolutePath))
    val wd: WorkflowDefinition = ex2err(GrammarParser.readWorkflow(opts.workflowFile))
    val confSpecs: Seq[ConfigAssignment] = ex2err(opts.config_file.value match {
      case Some(confFile) => {
        err.println("Reading workflow configuration: %s".format(confFile))
        GrammarParser.readConfig(new File(confFile))
      }
      case None => opts.config_name.value match {
        case Some(confName) => {
          wd.configs.find{ case c: ConfigDefinition => c.name == Some(confName) } match {
            case Some(x) => x.lines
            case None => throw new RuntimeException("Configuration not found: %s".format(confName))
          }
        }
        case None => {
          // use anonymous config, if provided
          wd.configs.find{ case c: ConfigDefinition => c.name == None } match {
            case Some(x) => x.lines
            case None => Nil
          }
        }
      }
    }) ++ wd.globals
    
    val checker = new StaticChecker(conf, undeclaredBehavior=Warn, unusedBehavior=Warn)
    val (warnings, errors) = checker.check(wd)
    for(msg <- warnings) {
       err.println("%sWARNING: %s%s".format(conf.warnColor, msg, conf.resetColor))
    }
    for(msg <- errors) {
       err.println("%sERROR: %s%s".format(conf.errorColor, msg, conf.resetColor))
    }
    if(errors.size > 0) {
      System.exit(1)
    }
    
    val builder = new WorkflowBuilder(wd, confSpecs)
    val workflow: HyperWorkflow = ex2err(builder.build())
    
    // TODO: Check that all input files exist
    val dirs = {
      val workflowBaseDir = opts.workflowFile.getAbsoluteFile.getParentFile
      val (confNameOpt, confBaseDir) = opts.config_file.value match {
        case Some(confFile) => {
          val confName = Files.basename(confFile, ".conf")
          (Some(confName), new File(workflowBaseDir, confName))
        }
        case None => opts.config_name.value match {
          case Some(confName) => (Some(confName), new File(workflowBaseDir, confName))
          case None => (None, workflowBaseDir)
        }
      }
      new DirectoryArchitect(workflowBaseDir, confBaseDir, confNameOpt)
    }

    def colorizeDir(taskName: String, real: Realization, versions: WorkflowVersioner): String = {
      val ver = versions(taskName, real)
      "%s/%s%s%s/%s%s%s/%d".format(dirs.confBaseDir.getAbsolutePath,
                                   conf.taskNameColor, taskName, conf.resetColor,
                                   conf.realNameColor, real.toString, conf.resetColor,
                                   ver)
    }

    def colorizeDirs(list: Iterable[(String,Realization)], versions: WorkflowVersioner): Seq[String] = {
      list.toSeq.map{ case (name, real) => colorizeDir(name, real, versions) }
    }

    def visitAll(visitor: UnpackedDagVisitor,
                 versions: WorkflowVersioner,
                 plannedVertices: Set[(String,Realization)],
                 numCores: Int = 1) {
      
      workflow.unpackedWalker(plannedVertices=plannedVertices).foreach(numCores, { v: UnpackedWorkVert => {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v, versions)
        visitor.visit(task)
      }})
    }

    // Our dag is directed from antecedents toward their consequents
    // After an initial forward pass that uses a realization filter
    // to generate vertices whose realizations are part of the plan
    // so we need to make a second reverse pass on the unpacked DAG
    // to make sure all of the vertices contribute to a goal vertex
    val initVersioner = new MostRecentWorkflowVersioner(dirs)
    val plannedVertices: Set[(String,Realization)] = workflow.plans match {
      case Nil => {
        err.println("Using default one-off realization plan")
        Set.empty
      }
      case _ => {
        err.println("Finding hyperpaths contained in plan...")
        
        def getCandidates(branchFilter: Map[BranchPoint, Set[Branch]]) = {
          val numCores = 1
          // tasks only know about their parents in the form of (taskName, realization)
          // not as references to their realized tasks. this lets them get garbage collected
          // and reduces memory usage. however, we need all the candidate realized tasks on hand
          // (pre-filtered by realization, but not by goal vertex) so that we can make
          // a backward pass over the unpacked DAG
          val candidates = new mutable.HashMap[(String,Realization), RealTask]

          // this is the most important place for us to pass the filter to unpackedWalker!
          workflow.unpackedWalker(branchFilter).foreach(numCores, { v: UnpackedWorkVert => {
            val taskT: TaskTemplate = v.packed.value
            val task: RealTask = taskT.realize(v, initVersioner)
            candidates += (task.name, task.realization) -> task
          }})
          candidates
        }
        
        val vertexFilter = new mutable.HashSet[(String,Realization)]
        for(plan: RealizationPlan <- workflow.plans) {
          err.println("Finding vertices for plan: %s".format(plan.name))
          
          val candidates = getCandidates(plan.realizations)
          val fronteir = new mutable.Queue[RealTask]
          
          // initialize with all valid realizations of the goal vertex
          // (realizations have already been filtered during HyperDAG traversal)
          for(goalTask <- plan.goalTasks) {
            val goalRealTasks: Iterable[RealTask] = candidates.filter{case ((tName, _), _) => tName == goalTask}.map(_._2)
            err.println("Found %d realizations of goal task %s: %s".format(goalRealTasks.size, goalTask, goalRealTasks.map{_.realization}.mkString(" ")))
            fronteir ++= goalRealTasks
          }
          
          val seen = new mutable.HashSet[RealTask]
          while(fronteir.size > 0) {
            val task: RealTask = fronteir.dequeue
            //err.println("Tracing back from task " + task)
            // add all parents (aka antecedents) to frontier
            if(!seen(task)) {
              try {
                val antTasks: Set[RealTask] = task.antecedents
                  .filter{ case (taskName, _) => taskName != WorkflowBuilder.CONFIG_TASK_DEF.name }
                  .map{case (taskName, real) => candidates(taskName, real)}
                fronteir ++= antTasks
              } catch {
                case e: NoSuchElementException => throw new RuntimeException("Error while trying to find antecedent tasks of %s".format(task), e)
              }
            }
            // mark this task as seen
            seen += task
          }
          
          // everything we saw is required to execute this realization plan to its goal vertices
          err.println("Found %d vertices implied by realization plan %s".format(seen.size, plan.name))
          vertexFilter ++= seen.map{task => (task.name, task.realization)}
        }
        err.println("Union of all planned vertices has size %d".format(vertexFilter.size))
        vertexFilter
      }
    }
      
    err.println("Checking for completed steps...")
    err.println("Planned: " + plannedVertices)
    // TODO: Refactor a bit? Only return the proper versioner? Make into on-demand method?
    val (cc: CompletionChecker, versions: ExecutionVersioner) = {
      val cc = new CompletionChecker(dirs, initVersioner)
      visitAll(cc, initVersioner, plannedVertices)
      (cc, new ExecutionVersioner(cc.completedVersions, initVersioner.nextVersion))
    }
    
    def getPackageVersions() = {
      val packageFinder = new PackageFinder(conf, dirs, versions, cc.todo, workflow.packageDefs)
      visitAll(packageFinder, versions, plannedVertices)
      System.err.println("Found %d packages".format(packageFinder.packages.size))

      err.println("Checking for already built packages...")
      val packageVersions = new PackageVersioner(dirs, wd.versioners)
      packageVersions.findAlreadyBuilt(packageFinder.packages.toSeq)
      packageVersions
    }

    def list {
      for(v: UnpackedWorkVert <- workflow.unpackedWalker(plannedVertices=plannedVertices).iterator) {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v, versions)
        println("%s %s".format(task.name, task.realization))
        //println("Actual realization: " + v.realization)
      }
    }

    def env {
      if(opts.taskName == None) {
        opts.exitHelp("env requires a taskName", 1)
      }
      if(opts.realNames.size != 1) {
        opts.exitHelp("env requires one realization name", 1)
      }
      val goalTaskName = opts.taskName.get
      val goalRealName = opts.realNames.head

      // TODO: Dont' apply plan filter?
      // TODO: Apply filters so that we do much less work to get here
      var matchingTasks: Iterable[UnpackedWorkVert] = {
        workflow.unpackedWalker(plannedVertices=plannedVertices).iterator.filter{v: UnpackedWorkVert => v.packed.value.name == goalTaskName}
      }.toIterable
      err.println("Found %d vertices with matching task name".format(matchingTasks.size))
      
      var matchingReals: Iterable[RealTask] = {
        matchingTasks.map{v: UnpackedWorkVert => {
          val taskT: TaskTemplate = v.packed.value
          val task: RealTask = taskT.realize(v, versions)
          if(task.realization.toString == goalRealName) {
            Some(task)
          } else {
            None
          }
        }}.filter(_ != None).map(_.get)
      }
      err.println("Found %d vertices with matching realizations".format(matchingReals.size))
      
      val packageVersions = getPackageVersions()
      
      for(task: RealTask <- matchingReals) {
        val env = new FullTaskEnvironment(dirs, versions, packageVersions, task)
        for( (k,v) <- env.env) {
          println("%s=%s".format(k,v))
        }
      }
    }

    def markDone {
      if(opts.taskName == None) {
        opts.exitHelp("mark_done requires a taskName", 1)
      }
      if(opts.realNames.size < 1) {
        opts.exitHelp("mark_done requires realization names", 1)
      }
      val goalTaskName = opts.taskName.get
      val goalRealNames = opts.realNames.toSet

      // TODO: Apply filters so that we do much less work to get here
      for(v: UnpackedWorkVert <- workflow.unpackedWalker(plannedVertices=plannedVertices).iterator) {
        val taskT: TaskTemplate = v.packed.value
        if(taskT.name == goalTaskName) {
          val task: RealTask = taskT.realize(v, initVersioner)
          if(goalRealNames(task.realization.toString)) {
            val env = new TaskEnvironment(dirs, versions, task)
            if(CompletionChecker.isComplete(env)) {
              err.println("Task already complete: " + task.name + "/" + task.realization)
            } else {
              CompletionChecker.forceCompletion(env)
              err.println("Forced completion of task: " + task.name + "/" + task.realization)
            }
          }
        }
      }
    }

    def exec {
      if(cc.todo.isEmpty) {
        // TODO: Might need to re-run if any package versions have changed
        err.println("All tasks to complete -- nothing to do")
      } else {
        err.println("Finding packages...")
        val packageVersions = getPackageVersions()
        
        err.println("Checking inputs...")
        val inputChecker = new InputChecker(conf, dirs)
        visitAll(inputChecker, versions, plannedVertices)
        if(inputChecker.errors.size > 0) {
          for(msg <- inputChecker.errors) {
            err.println("%sERROR: %s%s".format(conf.errorColor, msg, conf.resetColor))
          }
          System.exit(1)
        }
        
        // TODO: Check package versions to see if any packages need rebuilding.

        err.println("Work plan:")
        for(packageName <- packageVersions.packagesToBuild) {
          err.println("%sBUILD:%s %s".format(conf.greenColor, conf.resetColor, packageName))
        }

        for( (task, real) <- cc.todo) {
          err.println("%sRUN:%s %s".format(conf.greenColor, conf.resetColor, colorizeDir(task, real, versions)))
        }
      
//        if(cc.partial.size > 0) {
//          err.print("Are you sure you want to DELETE all this partial output and then run the tasks above? [y/n] ") // user must still press enter
//        } else {
//        }

        val answer = if(opts.yes) {
          true
        } else {
          // note: user must still press enter
          err.print("Are you sure you want to run these %d tasks? [y/n] ".format(cc.todo.size))
          Console.readBoolean
        }
        
        answer match {
          case true => {
            err.println("Retreiving code and building...")
            val builder = new PackageBuilder(dirs, versions.workflowVersion, packageVersions)
            builder.build(packageVersions.packagesToBuild)

//            err.println("Removing partial output...")
//            visitAll(new PartialOutputRemover(conf, dirs, versions, cc.partial), initVersioner, plannedVertices)
            err.println("Executing tasks...")
            try {
              visitAll(new Executor(dirs, versions, packageVersions, workflow, plannedVertices, cc.completed, cc.todo), versions, plannedVertices, opts.jobs())
            } catch {
              case e: ExecutionException => {
                err.println("%sERROR: %s%s".format(conf.errorColor, e.getMessage, conf.resetColor))
              }
            }
          }
          case _ => err.println("Doing nothing")
        }
      }
    }

    def viz {
      err.println("Generating GraphViz dot visualization...")
      import ducttape.viz._
      println(GraphViz.compileXDot(WorkflowViz.toGraphViz(workflow, plannedVertices, initVersioner)))
    }

    def debugViz {
      err.println("Generating GraphViz dot visualization of MetaHyperDAG...")
      import ducttape.viz._
      println(workflow.dag.toGraphVizDebug)
    }

    // supports '*' as a task or realization
    def getVictims(taskToKill: String, realsToKill: Set[String]): OrderedSet[RealTask] = {
      val victims = new mutable.HashSet[(String,Realization)]
      val victimList = new MutableOrderedSet[RealTask]
      for(v: UnpackedWorkVert <- workflow.unpackedWalker(plannedVertices=plannedVertices).iterator) {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v, initVersioner)
        if(taskToKill == "*" || taskT.name == taskToKill) {
          if(realsToKill == Set("*") || realsToKill(task.realization.toString)) {
            //err.println("Found victim %s/%s".format(taskT.name, task.realizationName))
            // TODO: Store seqs instead?
            victims += ((task.name, task.realization))
            victimList += task
          }
        } else {
          // was this task invalidated by its parent?
          // TODO: Can we propagate this in a more natural way
          val isVictim = task.inputVals.exists{ inputVal => {
            val parent = (inputVal.srcTaskDef.name, inputVal.srcReal)
            victims(parent)
          }}
          if(isVictim) {
            //err.println("Found indirect victim %s/%s".format(task.name, task.realizationName))
            victims += ((task.name, task.realization))
            victimList += task
          }
        }
      }
      //  TODO: Fix OrderedSet with a companion object so that we can use filter
      val extantVictims = new MutableOrderedSet[RealTask]
      for(task <- victimList) {
        val taskEnv = new TaskEnvironment(dirs, initVersioner, task)
        if(taskEnv.where.exists) {
          extantVictims += task
        } else {
          err.println("No previous output for: %s/%s/%s".format(task.name, task.realization, task.version))
        }
      }
      extantVictims
    }

    // TODO: Don't apply plan filtering to invalidation? More generally, we should let the user choose baseline-only, baseline-one-offs, cross product, or plan
    def invalidate {
      if(opts.taskName == None) {
        opts.exitHelp("invalidate requires a taskName", 1)
      }
      if(opts.realNames.size < 1) {
        opts.exitHelp("invalidate requires realization names", 1)
      }
      val taskToKill = opts.taskName.get
      val realsToKill = opts.realNames.toSet
      err.println("Finding tasks to be invalidated: %s for realizations: %s".format(taskToKill, realsToKill))

      // 1) Accumulate the set of changes
      val victims: OrderedSet[RealTask] = getVictims(taskToKill, realsToKill)
      val victimList: Seq[(String,Realization)] = victims.toSeq.map{ task => (task.name, task.realization) }
      
      // 2) prompt the user
      err.println("About to mark all the following directories as invalid so that a new version will be re-run for them:")
      err.println(colorizeDirs(victimList, initVersioner).mkString("\n"))
      
      val answer = if(opts.yes) {
        'y'
      } else {
        // note: user must still press enter
        err.print("Are you sure you want to invalidate all these? [y/n] ")
        Console.readChar
      }
      
      answer match {
        case 'y' | 'Y' => victims.foreach(task => {
          err.println("Invalidating %s/%s/%s".format(task.name, task.realization.toString, task.version))
          CompletionChecker.invalidate(new TaskEnvironment(dirs, initVersioner, task))
        })
        case _ => err.println("Doing nothing")
      }
    }

    def purge {
      if(opts.taskName == None) {
        opts.exitHelp("purge requires a taskName", 1)
      }
      if(opts.realNames.size < 1) {
        opts.exitHelp("purge requires realization names", 1)
      }
      val taskToKill = opts.taskName.get
      val realsToKill = opts.realNames.toSet
      err.println("Finding tasks to be purged: %s for realizations: %s".format(taskToKill, realsToKill))

      // 1) Accumulate the set of changes
      val victimList: Seq[(String,Realization)] = getVictims(taskToKill, realsToKill).toSeq.map{ task => (task.name, task.realization) }
      
      // 2) prompt the user
      err.println("About to permenantly delete the following directories:")
      // TODO: Use directory architect here
      val absDirs = victimList.map{case (name, real) => {
        val ver = initVersioner(name, real)
        new File(dirs.confBaseDir, "%s/%s/%d".format(name,real,ver))
      }}
      err.println(colorizeDirs(victimList, initVersioner).mkString("\n"))
      
      val answer = if(opts.yes) {
        'y'
      } else {
        // note: user must still press enter
        err.print("Are you sure you want to delete all these? [y/n] ")
        Console.readChar
      }

      answer match {
        case 'y' | 'Y' => absDirs.foreach(f => { err.println("Deleting %s".format(f.getAbsolutePath)); Files.deleteDir(f) })
        case _ => err.println("Doing nothing")
      }
    }

    // TODO: Have run() function in each mode?
    ex2err(opts.mode match {
      case "list" => list
      case "env" => env
      case "mark_done" => markDone
      case "viz" => viz
      case "debug_viz" => debugViz
      case "invalidate" => invalidate
      case "purge" => purge
      case _ => exec
    })
  }
}
