import System._
import collection._
import sys.ShutdownHookThread
import java.io.File
import java.util.concurrent.ExecutionException
import ducttape.db.WorkflowDatabase
import ducttape.db.TaskInfo
import ducttape.db.PackageInfo
import ducttape.db.InputInfo
import ducttape.db.OutputInfo
import ducttape.db.ParamInfo
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
import ducttape.exec.ForceUnlocker
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.GrammarParser
import ducttape.syntax.StaticChecker
import ducttape.syntax.ErrorBehavior._
import ducttape.versioner._
import ducttape.workflow.builder.WorkflowBuilder
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.Realization
import ducttape.workflow.TaskTemplate
import ducttape.workflow.RealTask
import ducttape.workflow.BranchPoint
import ducttape.workflow.Branch
import ducttape.workflow.RealizationPlan
import ducttape.workflow.PlanPolicy
import ducttape.workflow.VertexFilter
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
import ducttape.util.LogUtils
import grizzled.slf4j.Logging
import ducttape.syntax.WorkflowChecker
import ducttape.workflow.VertexFilter

object Ducttape extends Logging {
  
  def main(args: Array[String]) {
    LogUtils.initJavaLogging()
    
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
    debug("Reading workflow from %s".format(opts.workflowFile.getAbsolutePath))
    val wd: WorkflowDefinition = {
      val workflowOnly = ex2err(GrammarParser.readWorkflow(opts.workflowFile))
      
      val confStuff: WorkflowDefinition = ex2err(opts.config_file.value match {
        case Some(confFile) => {
          // TODO: Make sure workflow doesn't have anonymous conf?
          workflowOnly.anonymousConfig match {
            case Some(c) => throw new FileFormatException("Workflow cannot define anonymous config block if config file is used", c)
            case None => ;
          }
          
          err.println("Reading workflow configuration: %s".format(confFile))
          GrammarParser.readConfig(new File(confFile))
        }
        case None => new WorkflowDefinition(Nil)
      })
      if (!confStuff.globals.isEmpty) {
        throw new FileFormatException("Please use 'config' instead of 'global' in configuration files",
            confStuff.globals.toList)
      }
      
      workflowOnly ++ confStuff
    }
    
    val confNameOpt = opts.config_file.value match {
      case Some(confFile) => Some(Files.basename(confFile, ".conf"))
      case None => opts.config_name.value match {
        case Some(confName) => Some(confName)
        case None => None
      }
    }
    
    val confSpecs: Seq[ConfigAssignment] = {
      opts.config_file.value match {
        // use anonymous conf from config file, if any
        case Some(_) => wd.anonymousConfig match {
          case Some(c) => c.lines
          case None => Nil
        }
        case None => {
          // otherwise, use the conf the user requested, if any
          confNameOpt match {
            case Some(name) => {
              wd.configs.find { case c: ConfigDefinition => c.name == confNameOpt } match {
                case Some(x) => x.lines
                case None => throw new DucttapeException("Configuration not found: %s".format(name))
              }
            }
            case None => wd.anonymousConfig match {
              case Some(c) => c.lines
              case None => Nil
            }
          }
        }
      }
    } ++ wd.globals
    
    // XXX Remove as soon as Jon's current experiments are done - this is a hack
    {
      confSpecs.map(_.spec).find { spec => spec.name == "ducttape_branchpoint_delimiter" } match {
        case Some(spec) => spec.rval match {
          case lit: Literal => Realization.delimiter=lit.value.trim().toLowerCase.slice(0, 1)
          case _ => throw new FileFormatException("ducttape_branchpoint_delimiter directive must be a literal", spec)
        }
        case None => ()
      }
    }
    
    val flat: Boolean = ex2err {
      confSpecs.map(_.spec).find { spec => spec.name == "ducttape_structure" } match {
        case Some(spec) => spec.rval match {
          case lit: Literal => lit.value.trim().toLowerCase match {
            case "flat" => true
            case "hyper" => false
            case _ => throw new FileFormatException("ducttape_structure directive must be either 'flat' or 'hyper'", spec) 
          }
          case _ => throw new FileFormatException("ducttape_structure directive must be a literal", spec)
        }
        case None => false // not flat by default (hyper)
      }
    }
    if (flat) System.err.println("Using structure: flat")
        
    // TODO: Make sure conf specs include the workflow itself!
    
    implicit val dirs: DirectoryArchitect = {
      
      val workflowBaseDir: File = {
        opts.output.value match {
          // output directory was specified on the command line: use that first
          case Some(outputDir) => new File(outputDir)
          case None => {
            val confOutputDir = confSpecs.map(_.spec).find { spec => spec.name == "ducttape_output" }
            confOutputDir match {
              case Some(spec) => spec.rval match {
                // output directory was specified in the configuration file: use that next
                case lit: Literal => new File(lit.value.trim())
                case _ => throw new FileFormatException("ducttape_output directive must be a literal", spec)
              }
              // if unspecified, use PWD as the output directory
              case None => Environment.PWD
            }
          }
        }
      }
      new DirectoryArchitect(flat, workflowBaseDir, confNameOpt)
    }
    
    val builtins: Seq[WorkflowDefinition] = BuiltInLoader.load(dirs.builtinsDir)
    
    // pass 1 error checking: directly use workflow AST
    {
      val (warnings, errors) = {
        val bashChecker = new StaticChecker(undeclaredBehavior=Warn, unusedBehavior=Warn)
        val (warnings1, errors1) = bashChecker.check(wd)
        
        val workflowChecker = new WorkflowChecker(wd, confSpecs, builtins)
        val (warnings2, errors2) = workflowChecker.check()
        (warnings1 ++ warnings2, errors1 ++ errors2)
      }
      for (e: FileFormatException <- warnings) {
        ErrorUtils.prettyPrintError(e, prefix="WARNING", color=conf.warnColor)
      }
      for (e: FileFormatException <- errors) {
        ErrorUtils.prettyPrintError(e, prefix="ERROR", color=conf.errorColor)
      }
      if (warnings.size > 0) System.err.println("%d warnings".format(warnings.size))
      if (errors.size > 0) System.err.println("%d errors".format(errors.size))
      if (errors.size > 0) {
        exit(1)
      }
    }
    
    val builder = new WorkflowBuilder(wd, confSpecs, builtins)
    val workflow: HyperWorkflow = ex2err(builder.build())
    
    // Our dag is directed from antecedents toward their consequents
    // After an initial forward pass that uses a realization filter
    // to generate vertices whose realizations are part of the plan
    // so we need to make a second reverse pass on the unpacked DAG
    // to make sure all of the vertices contribute to a goal vertex
    
    def getPlannedVertices(): PlanPolicy = {
      val planPolicy: PlanPolicy = Plans.getPlannedVertices(workflow)
      planPolicy match {
        case VertexFilter(plannedVertices) => {
          System.err.println("Planned %s vertices".format(plannedVertices.size))
        }
        case _ => ;
      }

      // pass 2 error checking: use unpacked workflow
      {
        val workflowChecker = new WorkflowChecker(wd, confSpecs, builtins)
        val (warnings, errors) = workflowChecker.checkUnpacked(workflow, planPolicy)
        for (e: FileFormatException <- warnings) {
          ErrorUtils.prettyPrintError(e, prefix="WARNING", color=conf.warnColor)
        }
        for (e: FileFormatException <- errors) {
          ErrorUtils.prettyPrintError(e, prefix="ERROR", color=conf.errorColor)
        }
        if (warnings.size > 0) System.err.println("%d warnings".format(warnings.size))
        if (errors.size > 0) System.err.println("%d errors".format(errors.size))
        if (errors.size > 0) {
          exit(1)
        }
      }

      planPolicy
    }
            
    // Check version information
    val history = WorkflowVersionHistory.load(dirs.versionHistoryDir)
    err.println("Have %d previous workflow versions".format(history.prevVersion))
      
    def getCompletedTasks(planPolicy: PlanPolicy): CompletionChecker = {
      System.err.println("Checking for completed steps...")
      def msgCallback(msg: String) = System.err.println(msg)
      Visitors.visitAll(workflow, new CompletionChecker(dirs, msgCallback), planPolicy)
    }
    
    def getPackageVersions(cc: Option[CompletionChecker], planPolicy: PlanPolicy) = {
      val packageFinder = new PackageFinder(cc.map(_.todo), workflow.packageDefs)
      Visitors.visitAll(workflow, packageFinder, planPolicy)
      System.err.println("Found %d packages".format(packageFinder.packages.size))

      err.println("Checking for already built packages...")
      val packageVersions = new PackageVersioner(dirs, workflow.versioners)
      packageVersions.findAlreadyBuilt(packageFinder.packages.toSeq)
      packageVersions
    }

    def list {
      val planPolicy = getPlannedVertices()
      for (v: UnpackedWorkVert <- workflow.unpackedWalker(planPolicy).iterator) {
        val taskT: TaskTemplate = v.packed.value.get
        val task: RealTask = taskT.realize(v)
        println("%s %s".format(task.name, task.realization))
        //println("Actual realization: " + v.realization)
      }
    }
    
    // explain why certain realizations weren't generated (it's not always obvious)
    def explain {
      // TODO: More memory efficient uniquing strategy?
      val seen = new mutable.HashSet[(Option[String],String,String)]
      val have = new mutable.HashMap[String,mutable.HashSet[String]]
      def explainCallback(planName: Option[String], vertexName: =>String, msg: =>String, accepted: Boolean) {
        if (accepted) {
          have.getOrElseUpdate(vertexName, new mutable.HashSet[String]) += msg
        } else {
          if (!seen( (planName, vertexName, msg) )) {
            System.err.println("%s%s%s: %s%s%s: %s".format(
              conf.greenColor, planName.getOrElse("Anonymous"), conf.resetColor,
              conf.taskColor, vertexName, conf.resetColor,
              msg))
            seen += ((planName, vertexName, msg))
          }
        }
      }
      Plans.getPlannedVertices(workflow, explainCallback, errorOnZeroTasks=false)

      System.err.println("Accepted realizations: ")
      for ( (vertex, reals) <- have.toSeq.sortBy(_._1)) {
        System.err.println("%s%s%s".format(conf.taskColor, vertex, conf.resetColor))
        for (real <- reals) {
          System.err.println("  %s".format(real))
        }
      }
    }

    def markDone {
      val planPolicy = getPlannedVertices()
      if (opts.taskName == None) {
        opts.exitHelp("mark_done requires a taskName", 1)
      }
      if (opts.realNames.size < 1) {
        opts.exitHelp("mark_done requires realization names", 1)
      }
      val goalTaskName = opts.taskName.get
      val goalRealNames = opts.realNames.toSet

      // TODO: Apply filters so that we do much less work to get here
      for (v: UnpackedWorkVert <- workflow.unpackedWalker(planPolicy).iterator) {
        val taskT: TaskTemplate = v.packed.value.get
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
      val planPolicy = getPlannedVertices()
      
      err.println("Generating GraphViz dot visualization...")
      import ducttape.viz._
      println(GraphViz.compileXDot(WorkflowViz.toGraphViz(workflow, planPolicy)))
    }

    def debugViz {
      err.println("Generating GraphViz dot visualization of MetaHyperDAG...")
      import ducttape.viz._
      println(workflow.dag.toGraphVizDebug)
    }

    // supports '*' as a task or realization
    def getVictims(taskToKill: String,
                   realsToKill: Set[String],
                   planPolicy: PlanPolicy): OrderedSet[RealTask] = {
      
      val victims = new mutable.HashSet[(String,Realization)]
      val victimList = new MutableOrderedSet[RealTask]
      for (v: UnpackedWorkVert <- workflow.unpackedWalker(planPolicy).iterator) {
        val taskT: TaskTemplate = v.packed.value.get
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
          val isVictim = task.antecedents.exists { case (srcName, srcReal) =>
            val parent = (srcName, srcReal)
            victims(parent)
          }
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
      
      val planPolicy = getPlannedVertices()
      
      err.println("Finding tasks to be invalidated: %s for realizations: %s".format(taskToKill, realsToKill))

      // 1) Accumulate the set of changes
      val victims: OrderedSet[RealTask] = getVictims(taskToKill, realsToKill, planPolicy)
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
      
      val planPolicy = getPlannedVertices()
      
      err.println("Finding tasks to be purged: %s for realizations: %s".format(taskToKill, realsToKill))

      // 1) Accumulate the set of changes
      val victimList: Seq[RealTask] = getVictims(taskToKill, realsToKill, planPolicy).toSeq
      
      // 2) prompt the user
      import ducttape.cli.ColorUtils.colorizeDirs
      err.println("About to permenantly delete the following directories:")
      val absDirs: Seq[File] = victimList.map { task: RealTask => dirs.assignDir(task) }
      err.println(colorizeDirs(victimList).mkString("\n"))
      
      val answer = if (opts.yes) {
        'y'
      } else {
        // note: user must still press enter
        err.print("Are you sure you want to delete all these? [y/n] ")
        Console.readChar
      }

      answer match {
        case 'y' | 'Y' => absDirs.foreach { f: File => err.println("Deleting %s".format(f.getAbsolutePath)); Files.deleteDir(f) }
        case _ => err.println("Doing nothing")
      }
    }

    // TODO: Have run() function in each mode?
    ex2err(opts.mode match {
      case "list" => list
      case "explain" => explain
      case "env" => {
        val planPolicy: PlanPolicy = getPlannedVertices()
        val packageVersions = getPackageVersions(None, planPolicy)
        EnvironmentMode.run(workflow, planPolicy, packageVersions)
      }
      case "commands" => {
        val planPolicy: PlanPolicy = getPlannedVertices()
        val packageVersions = getPackageVersions(None, planPolicy)
        EnvironmentMode.run(workflow, planPolicy, packageVersions, showCommands=true)        
      }
      case "mark_done" => markDone
      case "viz" => viz
      case "debug_viz" => debugViz
      case "invalidate" => invalidate
      case "purge" => purge
      case "populate" => {
        System.err.println("Repopulating status database...")
        val db = new WorkflowDatabase(dirs.dbFile)
        db.addTask(new TaskInfo("tok", "Baseline.baseline", "running",  Seq(), Seq(new InputInfo("file", new File("/some/path"), None)), Seq(), Seq()))
        db.addTask(new TaskInfo("extract_gra_dev", "Baseline.baseline", "blocked", Seq(), Seq(), Seq(), Seq()))
        db.addTask(new TaskInfo("extract_gra_test", "Baseline.baseline", "blocked", Seq(), Seq(), Seq(), Seq()))
        db.addTask(new TaskInfo("tune", "Baseline.baseline", "done", Seq(), Seq(), Seq(), Seq()))
        db.addTask(new TaskInfo("decode", "Baseline.baseline", "failed", Seq(), Seq(), Seq(), Seq()))
        db.commit()
      }
      case "status" => {
        val db = new WorkflowDatabase(dirs.dbFile)
        for (task: TaskInfo <- db.getTasks) {
          System.out.println("%s/%s: %s".format(task.name, task.realName, task.status))
          for (input: InputInfo <- task.inputs) {
            System.out.println("  < %s: %s".format(input.name, input.path))
          }
          for (output: OutputInfo <- task.outputs) {
            System.out.println("  > %s: %s".format(output.name, output.path))
          }
          for (param: ParamInfo <- task.params) {
            System.out.println("  :: %s: %s".format(param.name, param.value))
          }
        }
      }
      case "unlock" => {
        val planPolicy = getPlannedVertices()
        val cc = getCompletedTasks(planPolicy)

        if (cc.locked.size > 0) {
          import ducttape.cli.ColorUtils.colorizeDir
          System.err.println("Remove locks:")
          for ( (task, real) <- cc.locked) {
            System.err.println("%sUNLOCK:%s %s".format(conf.greenColor, conf.resetColor, colorizeDir(task, real)))
          }
        }
          
        val answer = if (opts.yes) {
          true
        } else {
          // note: user must still press enter
          if (cc.locked.size > 0) {
            System.err.print("Are you sure you want to FORCE UNLOCK these %d tasks? (Only do this if you sure no other process is using them) [y/n] ".format(cc.todo.size))
            Console.readBoolean
          } else {
            false
          }
        }
        
        answer match {
          case true => Visitors.visitAll(workflow, new ForceUnlocker(dirs, todo=cc.locked), planPolicy)
          case _ => System.err.println("Doing nothing")
        }
       }
      case "exec" | _ => {
        val planPolicy = getPlannedVertices()
        val cc = getCompletedTasks(planPolicy)
        ExecuteMode.run(workflow, cc, planPolicy, history, { () => getPackageVersions(Some(cc), planPolicy) })
      }
    })
  }
}
