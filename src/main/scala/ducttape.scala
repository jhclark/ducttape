import System._
import collection._
import java.io.File
import java.util.concurrent.ExecutionException
import ducttape.exec.CompletionChecker
import ducttape.exec.Executor
import ducttape.exec.InputChecker
import ducttape.exec.PidWriter
import ducttape.exec.PackageBuilder
import ducttape.exec.PackageFinder
import ducttape.exec.TaskEnvironment
import ducttape.exec.UnpackedDagVisitor
import ducttape.exec.DirectoryArchitect
import ducttape.exec.PackageVersioner
import ducttape.exec.FullTaskEnvironment
import ducttape.exec.PartialOutputMover
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
import ducttape.util.Environment
import ducttape.workflow.BuiltInLoader
import ducttape.syntax.FileFormatException
import ducttape.util.DucttapeException
import ducttape.util.BashException

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

object Ducttape {

  import com.frugalmechanic.optparse._
  class Mode(val name: String, val desc: String) extends OptParse {
    def optCount = allOpts.size
    def unapply(name: String) = if(name == this.name) Some(name) else None
  }

  def exit(status: Int) {
    // If using an interactive console,
    if (java.lang.System.console() != null) {
      // reset colors to system defaults
      err.print(Console.RESET)
    }
    
    // Exit with appropriate status
    sys.exit(status)
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
      
      Ducttape.exit(code)
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
    implicit val conf = new Config
    val opts = new Opts(conf, args)
    if(opts.no_color || !Environment.hasTTY) {
      conf.clearColors()
    }
    
    err.println("%sDuctTape v0.2".format(conf.headerColor))
    err.println("%sBy Jonathan Clark".format(conf.byColor))
    err.println(conf.resetColor)

    // format exceptions as nice error messages
    def ex2err[T](func: => T): T = {
      
      def exitError(e: Exception) = {
        err.println("%sERROR: %s".format(conf.errorColor, e.getMessage))
        Ducttape.exit(1)
        throw new Error("Unreachable") // make the compiler happy
      }
      
      try { func } catch {
        case e: FileFormatException => {
          err.println("%sERROR: %s%s".format(conf.errorColor, e.getMessage, conf.resetColor))
          for( (file: File, line: Int, col: Int, untilLine: Int) <- e.refs) {
            err.println("%s%s:%d%s".format(conf.errorLineColor, file.getAbsolutePath, line, conf.resetColor))
            val badLines = Files.read(file).drop(line-1).take(line-untilLine+1)
            err.println(conf.errorScriptColor + badLines.mkString("\n"))
            err.println(" " * (col-2) + "^")
          }
          Ducttape.exit(1)
          throw new Error("Unreachable") // make the compiler happy
        }
        case e: BashException => exitError(e)
        case e: DucttapeException => exitError(e)
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
            case None => throw new DucttapeException("Configuration not found: %s".format(confName))
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
    
    val flat: Boolean = ex2err {
      confSpecs.map(_.spec).find{ spec => spec.name == "ducttape_structure" } match {
        case Some(spec) => spec.rval match {
          case lit: Literal => lit.value.trim().toLowerCase match {
            case "flat" => true
            case "hyper" => false
            case _ => throw new FileFormatException("ducttape stuctue directive must be either 'flat' or 'hyper'", spec) 
          }
          case _ => throw new FileFormatException("ducttape stucture directive must be a literal", spec)
        }
        case None => false // not flat by default (hyper)
      }
    }
    if (flat) System.err.println("Using structure: flat")
        
    implicit val dirs: DirectoryArchitect = {
      val workflowBaseDir = opts.workflowFile.getAbsoluteFile.getParentFile
      val confNameOpt = opts.config_file.value match {
        case Some(confFile) => Some(Files.basename(confFile, ".conf"))
        case None => opts.config_name.value match {
          case Some(confName) => Some(confName)
          case None => None
        }
      }
      new DirectoryArchitect(flat, workflowBaseDir, confNameOpt)
    }
    
    val builtins: Seq[WorkflowDefinition] = BuiltInLoader.load(dirs.builtinsDir)
    
    val checker = new StaticChecker(undeclaredBehavior=Warn, unusedBehavior=Warn)
    val (warnings, errors) = checker.check(wd)
    for(msg <- warnings) {
       err.println("%sWARNING: %s%s".format(conf.warnColor, msg, conf.resetColor))
    }
    for(msg <- errors) {
       err.println("%sERROR: %s%s".format(conf.errorColor, msg, conf.resetColor))
    }
    if(errors.size > 0) {
      Ducttape.exit(1)
    }
    
    val builder = new WorkflowBuilder(wd, confSpecs, builtins)
    val workflow: HyperWorkflow = ex2err(builder.build())

    // Check version information
    val history = WorkflowVersionHistory.load(dirs.versionHistoryDir)
    err.println("Have %d previous workflow versions".format(history.prevVersion))
    
    def colorizeDir(taskName: String, real: Realization)
                   (implicit dirs: DirectoryArchitect, conf: Config): String = {
      val x = "%s/%s%s%s".format(dirs.confBaseDir.getAbsolutePath, conf.taskNameColor, taskName, conf.resetColor)           
      if (dirs.flat) {
        x
      } else {
        x + "/%s%s%s".format(conf.realNameColor, real.toString, conf.resetColor)
      }
    }
  
    def colorizeDirs(list: Iterable[RealTask])
                    (implicit dirs: DirectoryArchitect, conf: Config): Seq[String] = {
      list.toSeq.map{ task => colorizeDir(task.name, task.realization) }
    }
    
    def visitAll[A <: UnpackedDagVisitor](
        visitor: A,
        plannedVertices: Set[(String,Realization)],
        numCores: Int = 1): A = {
      
      workflow.unpackedWalker(plannedVertices=plannedVertices).foreach(numCores, { v: UnpackedWorkVert => {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v)
        visitor.visit(task)
      }})
      visitor
    }

    // TODO: Move this to a separate file
    // Our dag is directed from antecedents toward their consequents
    // After an initial forward pass that uses a realization filter
    // to generate vertices whose realizations are part of the plan
    // so we need to make a second reverse pass on the unpacked DAG
    // to make sure all of the vertices contribute to a goal vertex
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
            val task: RealTask = taskT.realize(v)
            candidates += (task.name, task.realization) -> task
          }})
          candidates
        }
        
        val vertexFilter = new mutable.HashSet[(String,Realization)]
        for (plan: RealizationPlan <- workflow.plans) {
          err.println("Finding vertices for plan: %s".format(plan.name))
          
          val candidates = getCandidates(plan.realizations)
          val fronteir = new mutable.Queue[RealTask]
          
          // initialize with all valid realizations of the goal vertex
          // (realizations have already been filtered during HyperDAG traversal)
          for (goalTask <- plan.goalTasks) {
            val goalRealTasks: Iterable[RealTask] = candidates.filter{case ((tName, _), _) => tName == goalTask}.map(_._2)
            err.println("Found %d realizations of goal task %s: %s".format(goalRealTasks.size, goalTask, goalRealTasks.map{_.realization}.mkString(" ")))
            fronteir ++= goalRealTasks
          }
          
          val seen = new mutable.HashSet[RealTask]
          while (fronteir.size > 0) {
            val task: RealTask = fronteir.dequeue
            //err.println("Tracing back from task " + task)
            // add all parents (aka antecedents) to frontier
            if (!seen(task)) {
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
    if (plannedVertices.size > 0) err.println("Planned: " + plannedVertices)
    // TODO: Refactor a bit? Only return the proper versioner? Make into on-demand method?
    val cc: CompletionChecker = visitAll(new CompletionChecker(dirs), plannedVertices)
    //val workflowVersion = XXX
    
    def getPackageVersions() = {
      val packageFinder = new PackageFinder(dirs, cc.todo, workflow.packageDefs)
      visitAll(packageFinder, plannedVertices)
      System.err.println("Found %d packages".format(packageFinder.packages.size))

      err.println("Checking for already built packages...")
      val packageVersions = new PackageVersioner(dirs, workflow.versioners)
      packageVersions.findAlreadyBuilt(packageFinder.packages.toSeq)
      packageVersions
    }

    def list {
      for(v: UnpackedWorkVert <- workflow.unpackedWalker(plannedVertices=plannedVertices).iterator) {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v)
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
          val task: RealTask = taskT.realize(v)
          if(task.realization.toString == goalRealName) {
            System.err.println("My parents are: " + v.parentRealizations)
            Some(task)
          } else {
            None
          }
        }}.filter(_ != None).map(_.get)
      }
      err.println("Found %d vertices with matching realizations".format(matchingReals.size))
      
      val packageVersions = getPackageVersions()
      
      for(task: RealTask <- matchingReals) {
        val env = new FullTaskEnvironment(dirs, packageVersions, task)
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
          val task: RealTask = taskT.realize(v)
          if(goalRealNames(task.realization.toString)) {
            val env = new TaskEnvironment(dirs, task)
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
        val inputChecker = new InputChecker(dirs)
        visitAll(inputChecker, plannedVertices)
        if(inputChecker.errors.size > 0) {
          for(msg <- inputChecker.errors) {
            err.println("%sERROR: %s%s".format(conf.errorColor, msg, conf.resetColor))
          }
          Ducttape.exit(1)
        }
        
        // TODO: Check package versions to see if any packages need rebuilding.

        // TODO: Check for existing PID lock files from some other process...x
        
        err.println("Work plan:")
        for( (task, real) <- cc.broken) {
          err.println("%sDELETE:%s %s".format(conf.redColor, conf.resetColor, colorizeDir(task, real)))
        }
        for( (task, real) <- cc.partial) {
          err.println("%sMOVE TO ATTIC:%s %s".format(conf.redColor, conf.resetColor, colorizeDir(task, real)))
        }
        for(packageName <- packageVersions.packagesToBuild) {
          err.println("%sBUILD:%s %s".format(conf.greenColor, conf.resetColor, packageName))
        }
        for( (task, real) <- cc.todo) {
          err.println("%sRUN:%s %s".format(conf.greenColor, conf.resetColor, colorizeDir(task, real)))
        }

        val answer = if(opts.yes) {
          true
        } else {
          // note: user must still press enter
          if(cc.partial.size > 0) {
            err.print("Are you sure you want to MOVE all this partial output to the attic and then run these %d tasks? [y/n] ".format(cc.todo.size))
          } else {
            err.print("Are you sure you want to run these %d tasks? [y/n] ".format(cc.todo.size))
          }
          Console.readBoolean
        }
        
        answer match {
          case true => {
            // create a new workflow version
            val configFile: Option[File] = opts.config_file.value.map(new File(_))
            val myVersion: WorkflowVersionInfo = WorkflowVersionInfo.create(dirs, opts.workflowFile, configFile, history)
            
            err.println("Retreiving code and building...")
            val builder = new PackageBuilder(dirs, packageVersions)
            builder.build(packageVersions.packagesToBuild)

            err.println("Moving previous partial output to the attic...")
            // NOTE: We get the version of each failed attempt from its version file (partial). Lacking that, we kill it (broken).
            visitAll(new PartialOutputMover(dirs, cc.partial, cc.broken), plannedVertices)
            
            // Make a pass after moving partial output to write output files
            // claiming those directories as ours so that we can later start another ducttape process
            visitAll(new PidWriter(dirs, myVersion, cc.todo), plannedVertices)

            err.println("Executing tasks...")
            try {
              visitAll(new Executor(dirs, packageVersions, workflow, plannedVertices, cc.completed, cc.todo), plannedVertices, opts.jobs())
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
      println(GraphViz.compileXDot(WorkflowViz.toGraphViz(workflow, plannedVertices)))
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
        val task: RealTask = taskT.realize(v)
        if(taskToKill == "*" || taskT.name == taskToKill) {
          if(realsToKill == Set("*") || realsToKill(task.realization.toString)) {
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
            victims += ((task.name, task.realization))
            victimList += task
          }
        }
      }
      //  TODO: Fix OrderedSet with a companion object so that we can use filter
      val extantVictims = new MutableOrderedSet[RealTask]
      for(task <- victimList) {
        val taskEnv = new TaskEnvironment(dirs, task)
        if(taskEnv.where.exists) {
          extantVictims += task
        } else {
          err.println("No previous output for: %s".format(task))
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
      val victimList: Seq[RealTask] = victims.toSeq
      
      // 2) prompt the user
      err.println("About to mark all the following directories as invalid so that a new version will be re-run for them:")
      err.println(colorizeDirs(victimList).mkString("\n"))
      
      val answer = if(opts.yes) {
        'y'
      } else {
        // note: user must still press enter
        err.print("Are you sure you want to invalidate all these? [y/n] ")
        Console.readChar
      }
      
      answer match {
        case 'y' | 'Y' => victims.foreach(task => {
          err.println("Invalidating %s".format(task))
          CompletionChecker.invalidate(new TaskEnvironment(dirs, task))
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
      val victimList: Seq[RealTask] = getVictims(taskToKill, realsToKill).toSeq
      
      // 2) prompt the user
      err.println("About to permenantly delete the following directories:")
      val absDirs: Seq[File] = victimList.map{task: RealTask => dirs.assignDir(task) }
      err.println(colorizeDirs(victimList).mkString("\n"))
      
      val answer = if(opts.yes) {
        'y'
      } else {
        // note: user must still press enter
        err.print("Are you sure you want to delete all these? [y/n] ")
        Console.readChar
      }

      answer match {
        case 'y' | 'Y' => absDirs.foreach{f: File => { err.println("Deleting %s".format(f.getAbsolutePath)); Files.deleteDir(f) }}
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
