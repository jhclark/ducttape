import System._
import collection._
import sys.ShutdownHookThread
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
import ducttape.cli.Config
import ducttape.cli.ErrorUtils
import ducttape.cli.Opts
import ducttape.cli.EnvironmentMode
import ducttape.cli.Plans
import ducttape.workflow.Visitors
import ducttape.cli.ExecuteMode

object Ducttape {
  
  def main(args: Array[String]) {
    implicit val conf = new Config
    implicit val opts = new Opts(conf, args)
    if (opts.no_color || !Environment.hasTTY) {
      conf.clearColors()
    }
    
    import ducttape.cli.ErrorUtils.ex2err
    ShutdownHookThread { // make sure we never leave the color in a bad state on exit
      println(conf.resetColor)
      System.err.println(conf.resetColor)
    }
    
    err.println("%sDuctTape v0.2".format(conf.headerColor))
    err.println("%sBy Jonathan Clark".format(conf.byColor))
    err.println(conf.resetColor)

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
    for (msg <- warnings) {
       err.println("%sWARNING: %s%s".format(conf.warnColor, msg, conf.resetColor))
    }
    for (msg <- errors) {
       err.println("%sERROR: %s%s".format(conf.errorColor, msg, conf.resetColor))
    }
    if (errors.size > 0) {
      exit(1)
    }
    
    val builder = new WorkflowBuilder(wd, confSpecs, builtins)
    val workflow: HyperWorkflow = ex2err(builder.build())

    // Check version information
    val history = WorkflowVersionHistory.load(dirs.versionHistoryDir)
    err.println("Have %d previous workflow versions".format(history.prevVersion))

    // Our dag is directed from antecedents toward their consequents
    // After an initial forward pass that uses a realization filter
    // to generate vertices whose realizations are part of the plan
    // so we need to make a second reverse pass on the unpacked DAG
    // to make sure all of the vertices contribute to a goal vertex
    
    val plannedVertices = Plans.getPlannedVertices(workflow)
      
    err.println("Checking for completed steps...")
    if (plannedVertices.size > 0) err.println("Planned: " + plannedVertices)
    // TODO: Refactor a bit? Only return the proper versioner? Make into on-demand method?
    val cc: CompletionChecker = Visitors.visitAll(workflow, new CompletionChecker(dirs), plannedVertices)
    //val workflowVersion = XXX
    
    def getPackageVersions() = {
      val packageFinder = new PackageFinder(dirs, cc.todo, workflow.packageDefs)
      Visitors.visitAll(workflow, packageFinder, plannedVertices)
      System.err.println("Found %d packages".format(packageFinder.packages.size))

      err.println("Checking for already built packages...")
      val packageVersions = new PackageVersioner(dirs, workflow.versioners)
      packageVersions.findAlreadyBuilt(packageFinder.packages.toSeq)
      packageVersions
    }

    def list {
      for (v: UnpackedWorkVert <- workflow.unpackedWalker(plannedVertices=plannedVertices).iterator) {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v)
        println("%s %s".format(task.name, task.realization))
        //println("Actual realization: " + v.realization)
      }
    }

    def markDone {
      if (opts.taskName == None) {
        opts.exitHelp("mark_done requires a taskName", 1)
      }
      if (opts.realNames.size < 1) {
        opts.exitHelp("mark_done requires realization names", 1)
      }
      val goalTaskName = opts.taskName.get
      val goalRealNames = opts.realNames.toSet

      // TODO: Apply filters so that we do much less work to get here
      for (v: UnpackedWorkVert <- workflow.unpackedWalker(plannedVertices=plannedVertices).iterator) {
        val taskT: TaskTemplate = v.packed.value
        if (taskT.name == goalTaskName) {
          val task: RealTask = taskT.realize(v)
          if (goalRealNames(task.realization.toString)) {
            val env = new TaskEnvironment(dirs, task)
            if (CompletionChecker.isComplete(env)) {
              err.println("Task already complete: " + task.name + "/" + task.realization)
            } else {
              CompletionChecker.forceCompletion(env)
              err.println("Forced completion of task: " + task.name + "/" + task.realization)
            }
          }
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
      for (v: UnpackedWorkVert <- workflow.unpackedWalker(plannedVertices=plannedVertices).iterator) {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v)
        if (taskToKill == "*" || taskT.name == taskToKill) {
          if (realsToKill == Set("*") || realsToKill(task.realization.toString)) {
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
          if (isVictim) {
            victims += ((task.name, task.realization))
            victimList += task
          }
        }
      }
      //  TODO: Fix OrderedSet with a companion object so that we can use filter
      val extantVictims = new MutableOrderedSet[RealTask]
      for (task <- victimList) {
        val taskEnv = new TaskEnvironment(dirs, task)
        if (taskEnv.where.exists) {
          extantVictims += task
        } else {
          err.println("No previous output for: %s".format(task))
        }
      }
      extantVictims
    }

    // TODO: Don't apply plan filtering to invalidation? More generally, we should let the user choose baseline-only, baseline-one-offs, cross product, or plan
    def invalidate {
      if (opts.taskName == None) {
        opts.exitHelp("invalidate requires a taskName", 1)
      }
      if (opts.realNames.size < 1) {
        opts.exitHelp("invalidate requires realization names", 1)
      }
      val taskToKill = opts.taskName.get
      val realsToKill = opts.realNames.toSet
      err.println("Finding tasks to be invalidated: %s for realizations: %s".format(taskToKill, realsToKill))

      // 1) Accumulate the set of changes
      val victims: OrderedSet[RealTask] = getVictims(taskToKill, realsToKill)
      val victimList: Seq[RealTask] = victims.toSeq
      
      // 2) prompt the user
      import ducttape.cli.ColorUtils.colorizeDirs
      err.println("About to mark all the following directories as invalid so that a new version will be re-run for them:")
      err.println(colorizeDirs(victimList).mkString("\n"))
      
      val answer = if (opts.yes) {
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
      if (opts.taskName == None) {
        opts.exitHelp("purge requires a taskName", 1)
      }
      if (opts.realNames.size < 1) {
        opts.exitHelp("purge requires realization names", 1)
      }
      val taskToKill = opts.taskName.get
      val realsToKill = opts.realNames.toSet
      err.println("Finding tasks to be purged: %s for realizations: %s".format(taskToKill, realsToKill))

      // 1) Accumulate the set of changes
      val victimList: Seq[RealTask] = getVictims(taskToKill, realsToKill).toSeq
      
      // 2) prompt the user
      import ducttape.cli.ColorUtils.colorizeDirs
      err.println("About to permenantly delete the following directories:")
      val absDirs: Seq[File] = victimList.map{task: RealTask => dirs.assignDir(task) }
      err.println(colorizeDirs(victimList).mkString("\n"))
      
      val answer = if (opts.yes) {
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
      case "env" => EnvironmentMode.run(workflow, plannedVertices, getPackageVersions())
      case "mark_done" => markDone
      case "viz" => viz
      case "debug_viz" => debugViz
      case "invalidate" => invalidate
      case "purge" => purge
      case _ => ExecuteMode.run(workflow, cc, plannedVertices, history, getPackageVersions)
    })
  }
}
