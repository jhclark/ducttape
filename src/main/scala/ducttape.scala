import System._
import collection._
import sys.ShutdownHookThread

import java.io.File
import java.util.concurrent.ExecutionException
import java.util.regex.Pattern

import ducttape.cli.Config
import ducttape.cli.Directives
import ducttape.cli.ErrorUtils
import ducttape.cli.ErrorUtils.ex2err
import ducttape.cli.Opts
import ducttape.cli.EnvironmentMode
import ducttape.cli.Plans
import ducttape.cli.RealizationGlob
import ducttape.cli.ExecuteMode
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
import ducttape.hyperdag.walker.Traversal
import ducttape.hyperdag.walker.Arbitrary
import ducttape.hyperdag.walker.BreadthFirst
import ducttape.hyperdag.walker.DepthFirst
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.GrammarParser
import ducttape.syntax.StaticChecker
import ducttape.syntax.Namespace
import ducttape.syntax.ErrorBehavior
import ducttape.syntax.ErrorBehavior._
import ducttape.workflow.builder.WorkflowBuilder
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.Realization
import ducttape.workflow.TaskTemplate
import ducttape.workflow.RealTask
import ducttape.workflow.VersionedTask
import ducttape.workflow.BranchPoint
import ducttape.workflow.Branch
import ducttape.workflow.RealizationPlan
import ducttape.workflow.PlanPolicy
import ducttape.workflow.VertexFilter
import ducttape.workflow.Types._
import ducttape.workflow.BuiltInLoader
import ducttape.workflow.VertexFilter
import ducttape.workflow.Visitors
import ducttape.versioner.WorkflowVersionInfo
import ducttape.versioner.WorkflowVersionHistory
import ducttape.syntax.FileFormatException
import ducttape.syntax.WorkflowChecker
import ducttape.util.Files
import ducttape.util.OrderedSet
import ducttape.util.MutableOrderedSet
import ducttape.util.Environment
import ducttape.util.LogUtils
import ducttape.util.DucttapeException
import ducttape.util.BashException
import ducttape.util.Globs
import ducttape.util.Shell

import grizzled.slf4j.Logging

object Ducttape extends Logging {
  
  def main(args: Array[String]) {
    LogUtils.initJavaLogging()

    implicit val opts = new Opts(args)
    if (opts.no_color || !Environment.hasTTY) {
      Config.clearColors()
    }

    ShutdownHookThread { // make sure we never leave the color in a bad state on exit
      println(Config.resetColor)
      System.err.println(Config.resetColor)
    }
    
    val ducttapeVersion: String = try { 
      Files.readJarResource("/version.info").head
    } catch {
      case _: Throwable => "(version unknown)"
    }
    err.println(s"ducttape $ducttapeVersion")
    err.println("by Jonathan Clark")

    // read user config before printing anything to screen
    val userConfigFile = new File(Environment.UserHomeDir, ".ducttape")
    debug(s"Checking for user config at: ${userConfigFile.getAbsolutePath}")
    val userConfig: WorkflowDefinition = if (userConfigFile.exists) {
      GrammarParser.readConfig(userConfigFile)
    } else {
      new WorkflowDefinition(Nil)
    }
    
    // make these messages optional with verbosity levels?
    debug(s"Reading workflow from ${opts.workflowFile.getAbsolutePath}")
    val wd: WorkflowDefinition = {
      val workflowOnly = ex2err(GrammarParser.readWorkflow(opts.workflowFile))
      
      val confStuff: WorkflowDefinition = ex2err(opts.config_file.value match {
        case Some(confFile) => {
          // TODO: Make sure workflow doesn't have anonymous conf?
          workflowOnly.anonymousConfig match {
            case Some(c) => throw new FileFormatException("Workflow cannot define anonymous config block if config file is used", c)
            case None => ;
          }
          
          err.println(s"Reading workflow configuration: ${confFile}")
          GrammarParser.readConfig(new File(confFile))
        }
        case None => new WorkflowDefinition(Nil)
      })
      
      workflowOnly ++ confStuff ++ userConfig
    }

    val confNameOpt = opts.config_file.value match {
      case Some(confFile) => Some(Files.basename(confFile, ".conf", ".tconf"))
      case None => None
    }
    
    // TODO: Can we remove this entirely now that config files have the same syntax as regular .tape files?
    val confSpecs: Seq[ConfigAssignment] = ex2err {
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
                case None => throw new DucttapeException(s"Configuration not found: ${name}")
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
    
    implicit val directives: Directives = ex2err {
      new Directives(confSpecs)
    }
        
    if (directives.flat) System.err.println("Using structure: flat")
        
    // TODO: Make sure conf specs include the workflow itself!
    
    implicit val dirs: DirectoryArchitect = {
      val workflowBaseDir: File = {
        opts.output.value match {
          // output directory was specified on the command line: use that first
          case Some(outputDir) => new File(outputDir)
          case None => directives.output match { // ducttape_output=...
            case Some(value) => new File(value)
            // if unspecified, use PWD as the output directory
            case None => Environment.PWD
          }
        }
      }
      new DirectoryArchitect(directives.flat, directives.versionedTasks, workflowBaseDir, confNameOpt)
    }
    
    val builtins: Seq[WorkflowDefinition] = BuiltInLoader.load(dirs.builtinsDir)
    
    // pass 1 error checking: directly use workflow AST
    {
      val undeclaredBehavior = ErrorBehavior.parse(directives.undeclared_vars, default=Warn)
      val unusedBehavior = ErrorBehavior.parse(directives.unused_vars, default=Warn)

      val (warnings, errors) = {
        val bashChecker = new StaticChecker(undeclaredBehavior, unusedBehavior)
        val (warnings1, errors1) = bashChecker.check(wd)
        
        val workflowChecker = new WorkflowChecker(wd, confSpecs, builtins, directives)
        val (warnings2, errors2) = workflowChecker.check()
        (warnings1 ++ warnings2, errors1 ++ errors2)
      }
      for (e: FileFormatException <- warnings) {
        ErrorUtils.prettyPrintError(e, prefix="WARNING", color=Config.warnColor)
      }
      for (e: FileFormatException <- errors) {
        ErrorUtils.prettyPrintError(e, prefix="ERROR", color=Config.errorColor)
      }
      if (warnings.size > 0) System.err.println(s"${warnings.size} warnings")
      if (errors.size > 0) System.err.println(s"${errors.size} errors")
      if (errors.size > 0) {
        exit(1)
      }
    }
    
    val builder = new WorkflowBuilder(wd, confSpecs, builtins)
    val workflow: HyperWorkflow = ex2err(builder.build())

    val traversal = opts.traversal.getOrElse("DepthFirst").toLowerCase() match {
      case "arbitrary"    => Arbitrary
      case "breadthfirst" => BreadthFirst
      case _              => DepthFirst
    }

    // Our dag is directed from antecedents toward their consequents
    // After an initial forward pass that uses a realization filter
    // to generate vertices whose realizations are part of the plan
    // so we need to make a second reverse pass on the unpacked DAG
    // to make sure all of the vertices contribute to a goal vertex
    
    def getVersionHistory(): WorkflowVersionHistory = {
      System.err.println("Loading workflow version history...")
      WorkflowVersionHistory.load(dirs.versionHistoryDir)
    }

    def getPrevWorkflowVersion(mode: String): WorkflowVersionInfo = {
      getVersionHistory().prevVersionInfo match {
        case Some(versionInfo) => {
          System.err.println(s"Using workflow version: ${versionInfo}")
          versionInfo
        }
        case None => throw new RuntimeException(s"No previous version history exists for this workflow, but '${mode}' mode requires a previous version")
      }
    }

    def getPlannedVertices(workflowVersion: WorkflowVersionInfo): PlanPolicy = {
      // pass in user-specified plan name -- iff it was specified by the user -- otherwise use all plans
      val planPolicy: PlanPolicy = Plans.getPlannedVertices(workflow, workflowVersion, planNames=opts.plans)
      planPolicy match {
        case VertexFilter(plannedVertices) => {
          System.err.println(s"Planned ${plannedVertices.size} vertices")
        }
        case _ => ;
      }

      // pass 2 error checking: use unpacked workflow
      {
        val workflowChecker = new WorkflowChecker(wd, confSpecs, builtins, directives)
        val (warnings, errors) = workflowChecker.checkUnpacked(workflow, planPolicy, workflowVersion)
        for (e: FileFormatException <- warnings) {
          ErrorUtils.prettyPrintError(e, prefix="WARNING", color=Config.warnColor)
        }
        for (e: FileFormatException <- errors) {
          ErrorUtils.prettyPrintError(e, prefix="ERROR", color=Config.errorColor)
        }
        if (warnings.size > 0) System.err.println(s"${warnings.size} warnings")
        if (errors.size > 0) System.err.println(s"${errors.size} errors")
        if (errors.size > 0) {
          exit(1)
        }
      }

      planPolicy
    }
            
    // Check version information
    val history = WorkflowVersionHistory.load(dirs.versionHistoryDir)
    err.println(s"Have ${history.prevVersion.getOrElse(0)} previous workflow versions")
    
    // this is a critical stage for versioning -- here, we decide whether or not
    // we have an acceptable existing version of a task or if we'll need a new
    // version for that task
    def getCompletedTasks(planPolicy: PlanPolicy, history: WorkflowVersionHistory): CompletionChecker = {
      
      // We need to check for any completed version of this task (that's not invalidated)
      // in *any* previous version (not just the most recent)
      // So we use a "union" version to check for this
      val unionVersion: WorkflowVersionInfo = history.union()

      history.prevVersion match {
        case Some(prev) => System.err.println(s"Checking for completed steps from versions through ${prev}...")
        case None => System.err.println("No previous workflow versions -- no tasks completed yet.")
      }
      def msgCallback(msg: String) = System.err.println(msg)
      val cc = new CompletionChecker(dirs, unionVersion, history.nextVersion, msgCallback)
      Visitors.visitAll(workflow, cc, planPolicy, unionVersion, traversal=traversal)
    }

    // get what packages are needed by the planned vertices and
    // what repo version of each of those we have built already (if any)    
    def getPackageVersions(cc: Option[CompletionChecker], planPolicy: PlanPolicy, workflowVersion: WorkflowVersionInfo) = {

      // find what packages are required by the planned vertices
      // TODO: Always return all packages when using auto_update?
      val packageFinder = new PackageFinder(cc.map(_.todo), workflow.packageDefs)
      Visitors.visitAll(workflow, packageFinder, planPolicy, workflowVersion)
      System.err.println(s"Found ${packageFinder.packages.size} packages")

      // now see what the repo version is for these packages
      // and also determine if they need to be rebuilt in execute mode
      err.println("Checking for already built packages...")
      val packageVersions = new PackageVersioner(dirs, workflow.versioners)
      packageVersions.findAlreadyBuilt(packageFinder.packages.toSeq)
      packageVersions
    }

    def list {
      val workflowVersion = WorkflowVersionInfo.createFake()
      val planPolicy = getPlannedVertices(workflowVersion)
      for (v: UnpackedWorkVert <- workflow.unpackedWalker(planPolicy).iterator) {
        val taskT: TaskTemplate = v.packed.value.get
        val task: VersionedTask = taskT.toRealTask(v).toVersionedTask(workflowVersion)
        // we don't actually need the versioned task here... yet.
        // TODO: but it might make sense to add a switch to list dependent versions, etc.
        println(s"${task.name} ${task.realization}")
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
            System.err.println(s"${Config.greenColor}${planName.getOrElse("Anonymous")}${Config.resetColor}: " +
                               s"${Config.taskColor}${vertexName}${Config.resetColor}: ${msg}")
            seen += ((planName, vertexName, msg))
          }
        }
      }
      val workflowVersion = WorkflowVersionInfo.createFake()
      Plans.getPlannedVertices(workflow, workflowVersion, explainCallback, errorOnZeroTasks=false)

      System.err.println("Accepted realizations: ")
      for ( (vertex, reals) <- have.toSeq.sortBy(_._1)) {
        System.err.println(s"${Config.taskColor}${vertex}${Config.resetColor}")
        for (real <- reals) {
          System.err.println(s"  ${real}")
        }
      }
    }

    def markDone {
      // TODO: Support more than just the most recent version via a command line option
      val workflowVersion = getPrevWorkflowVersion("mark_done")
      val planPolicy = getPlannedVertices(workflowVersion)
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
          val task: VersionedTask = taskT.toRealTask(v).toVersionedTask(workflowVersion)
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
      val workflowVersion = WorkflowVersionInfo.createFake()
      val planPolicy = getPlannedVertices(workflowVersion)
      
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
    // or globs containing interlaced '*' and '?' as tasks or realizations
    def getVictims(taskToKill: String,
                   realsToKill: Set[String],
                   planPolicy: PlanPolicy,
                   workflowVersion: WorkflowVersionInfo): OrderedSet[VersionedTask] = {
      
      val taskPattern: Pattern = Pattern.compile(Globs.globToRegex(taskToKill))
      val realPatterns: Seq[RealizationGlob] = realsToKill.toSeq.map(new RealizationGlob(_))

      val victims = new mutable.HashSet[(String,Realization)]
      val victimList = new MutableOrderedSet[VersionedTask]
      for (v: UnpackedWorkVert <- workflow.unpackedWalker(planPolicy).iterator) {
        val taskT: TaskTemplate = v.packed.value.get
        val task: VersionedTask = taskT.toRealTask(v).toVersionedTask(workflowVersion)
        val taskName: Namespace = taskT.name
        if (taskPattern.matcher(taskName.toString).matches) {
        //if (taskToKill == "*" || taskT.name == taskToKill) {
          //if (realsToKill == Set("*") || realsToKill(task.realization.toString)) {
          if (realPatterns.exists(_.matches(task.realization))) {
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
      val extantVictims = new MutableOrderedSet[VersionedTask]
      for (task <- victimList) {
        val taskEnv = new TaskEnvironment(dirs, task)
        if (taskEnv.where.exists) {
          extantVictims += task
        } else {
          err.println(s"No previous output for: ${task}")
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
      
      // TODO: Support more than just the most recent version via a command line option
      val workflowVersion = getPrevWorkflowVersion("invalidate")
      val planPolicy = getPlannedVertices(workflowVersion)
      
      err.println(s"Finding tasks to be invalidated: ${taskToKill} for realizations: ${realsToKill}")

      // 1) Accumulate the set of changes
      val victims: OrderedSet[VersionedTask] = getVictims(taskToKill, realsToKill, planPolicy, workflowVersion)
      val victimList: Seq[VersionedTask] = victims.toSeq
      
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
          err.println(s"Invalidating ${task}")
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
      
      // TODO: Support more than just the most recent version via a command line option
      val workflowVersion = getPrevWorkflowVersion("purge")
      val planPolicy = getPlannedVertices(workflowVersion)
      
      err.println(s"Finding tasks to be purged: ${taskToKill} for realizations: ${realsToKill}")

      // 1) Accumulate the set of changes
      val victimList: Seq[VersionedTask] = getVictims(taskToKill, realsToKill, planPolicy, workflowVersion).toSeq
      
      // 2) prompt the user
      import ducttape.cli.ColorUtils.colorizeDirs
      err.println("About to permenantly delete the following directories:")
      val absDirs: Seq[File] = victimList.map { task: VersionedTask => dirs.assignDir(task) }
      err.println(colorizeDirs(victimList).mkString("\n"))
      
      val answer = if (opts.yes) {
        'y'
      } else {
        // note: user must still press enter
        err.print("Are you sure you want to delete all these? [y/n] ")
        Console.readChar
      }

      answer match {
        case 'y' | 'Y' => absDirs.foreach { f: File =>
          err.println(s"Deleting ${f.getAbsolutePath}")
          Files.deleteDir(f)
        }
        case _ => err.println("Doing nothing")
      }
    }

    // TODO: Have run() function in each mode?
    ex2err(opts.mode match {
      case "list" => list
      case "explain" => explain
      case "env" => {
        // TODO: Allow user to get environment for arbitrary version of a task via CLI switch
        val workflowVersion = WorkflowVersionInfo.createFake()
        val planPolicy: PlanPolicy = getPlannedVertices(workflowVersion)
        val packageVersions = getPackageVersions(None, planPolicy, workflowVersion)
        EnvironmentMode.run(workflow, planPolicy, packageVersions, workflowVersion)
      }
      case "commands" => {
        // TODO: Allow user to get environment for arbitrary version of a task via CLI switch
        val workflowVersion = WorkflowVersionInfo.createFake()
        val planPolicy: PlanPolicy = getPlannedVertices(workflowVersion)
        val packageVersions = getPackageVersions(None, planPolicy, workflowVersion)
        EnvironmentMode.run(workflow, planPolicy, packageVersions, workflowVersion, showCommands=true)        
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
          System.out.println(s"${task.name}/${task.realName}: ${task.status}")
          for (input: InputInfo <- task.inputs) {
            System.out.println(s"  < ${input.name}: ${input.path}")
          }
          for (output: OutputInfo <- task.outputs) {
            System.out.println(s"  > ${output.name}: ${output.path}")
          }
          for (param: ParamInfo <- task.params) {
            System.out.println(s"  :: ${param.name}: ${param.value}")
          }
        }
      }
      case "summary" => {
        // 1) find which summary the user asked for (else use them all)
        //    -- just use them all for now
        //    -- we should also allow the user to specify which "plans" they would like us to enumerate
        //    -- we should also allow ther user to specify task globs on the command line

        // TODO: More static checking of summaries to disallow params and inputs (and even packages for now)

        // TODO: As a static pre-processing step, we should make sure summaries are referring to existing tasks...
        //wd.tasks.find { _.name == taskName } match {
        //  case None => throw new FileFormatException("Task '%s' not found. Required by summary '%s'".format(taskName, summary.name), summaryDef)
        //  case Some(taskDef) => {
        //  }
        //}
        
        // TODO: Support more than just the most recent version via a command line option
        val workflowVersion = getPrevWorkflowVersion("summary")
        val planPolicy = getPlannedVertices(workflowVersion)
        val history = getVersionHistory()
        val cc = getCompletedTasks(planPolicy, history)
        val packageVersions = getPackageVersions(None, planPolicy, workflowVersion)
        
        // 2) Run each summary block and store in a big table

        //    we also keep track of a) all branch points we've seen and produce one column in our output table for each
        //    and b) each branch seen for each of those branch points -- branch points having only a single branch will be omitted
        //    to make the control variables of each experiment more obvious
        val branchPointMap = new mutable.HashMap[BranchPoint, mutable.HashSet[Branch]]
        val labelSet = new mutable.HashSet[String]
        val results = new mutable.HashMap[(String,Realization), mutable.HashMap[String,String]]
        for (v: UnpackedWorkVert <- workflow.unpackedWalker(planPolicy).iterator) {
          val taskT: TaskTemplate = v.packed.value.get
          val task: VersionedTask = taskT.toRealTask(v).toVersionedTask(workflowVersion)

          for (summaryDef: SummaryDef <- wd.summaries) {
            for (ofDef: SummaryTaskDef <- summaryDef.ofs; if (ofDef.name == task.name && cc.completed( (task.name, task.realization) ))) {
              val taskEnv = new FullTaskEnvironment(dirs, packageVersions, task)
              val workDir = dirs.getTempActionDir("summary")
              Files.mkdirs(workDir)
                  
              val summaryOutputs: Seq[(String,String)] = ofDef.outputs.map { spec: Spec =>
                (spec.name, new File(workDir, spec.name).getAbsolutePath)
              }

              // TODO: Use all the same variables as a submitter?
              val env: Seq[(String,String)] = Seq( ("TASK_DIR", taskEnv.where.getAbsolutePath) ) ++ summaryOutputs ++ taskEnv.env

              // f) run the summary command
              val code = ofDef.commands.toString
              val stdPrefix = taskEnv.task.toString
              val stdoutFile = new File(workDir, "summary_stdout.txt")
              val stderrFile = new File(workDir, "summary_stderr.txt")
              val exitCodeFile = new File(workDir, "summary_exit_code.txt")

              err.println(s"Summarizing ${summaryDef.name}: ${task}")
              val exitCode = Shell.run(code, stdPrefix, workDir, env, stdoutFile, stderrFile)
              Files.write(s"${exitCode}", exitCodeFile)
              if (exitCode != 0) {
                throw new BashException(s"Summary '${summaryDef.name}' of ${taskEnv.task} failed")
              }
              
              // g) extract the result from the file and put it in our table
              for ( (outputName, path) <- summaryOutputs) {
                val lines: Seq[String] = Files.read(new File(path))
                if (lines.size != 1) {
                  throw new BashException(s"For summary '${summaryDef.name}', expected exactly one line in '${path}', but instead found ${lines.size}")
                }
                val label = outputName
                val result: String = lines(0)
                results.getOrElseUpdate( (task.name, task.realization), new mutable.HashMap[String,String] ) += label -> result
                for (branch <- task.realization.branches) {
                  branchPointMap.getOrElseUpdate(branch.branchPoint, new mutable.HashSet) += branch
                }
                labelSet += label
              }
              
              // h) cleanup
              Files.deleteDir(workDir)
            }
          }
        }

        // 3) print the table out in a nice tab-delimited format
        // first line is header
        val allBranchPoints = branchPointMap.keys.toSeq.sortBy(_.name)
        val (constantBranchPointMap, variableBranchPointMap)
          = branchPointMap.toSeq.sortBy(_._1.name).partition { case (bp, branches) => branches.size == 1 }
        for ( (bp, branches) <- constantBranchPointMap) {
          System.err.println(s"Constant branch point: ${bp}=${branches.head}".format(bp, branches.head))
        }
        val variableBranchPoints: Seq[BranchPoint] = variableBranchPointMap.map(_._1)
        System.err.println(s"Variable branch points: ${variableBranchPoints.mkString(" ")}")

        val labels: Seq[String] = labelSet.toSeq.sorted
        val header: Seq[String] = variableBranchPoints.map(_.name) ++ labels
        System.out.println(header.mkString("\t"))
        for ( ((taskName, real), values) <- results) {
          val branches: Map[BranchPoint, String] = real.branches.map { branch => (branch.branchPoint, branch.name) }.toMap
          val cols: Seq[String] = variableBranchPoints.map { bp => branches.getOrElse(bp, "--") } ++
            labels.map { label => values.getOrElse(label, "--") }
          System.out.println(cols.mkString("\t"))
        }
      }
      case "unlock" => {
        val workflowVersion = getPrevWorkflowVersion("unlock")
        val planPolicy = getPlannedVertices(workflowVersion)
        val history = getVersionHistory()
        val cc = getCompletedTasks(planPolicy, history)

        if (cc.locked.size > 0) {
          import ducttape.cli.ColorUtils.colorizeDir
          System.err.println("Remove locks:")
          for ( (task, real) <- cc.locked) {
            System.err.println(s"${Config.greenColor}UNLOCK:${Config.resetColor} ${colorizeDir(task, real)}")
          }
        }
          
        val answer = if (opts.yes) {
          true
        } else {
          // note: user must still press enter
          if (cc.locked.size > 0) {
            System.err.print(s"Are you sure you want to FORCE UNLOCK these ${cc.todo.size} tasks? (Only do this if you sure no other process is using them) [y/n] ")
            Console.readBoolean
          } else {
            false
          }
        }
        
        answer match {
          case true => Visitors.visitAll(workflow, new ForceUnlocker(dirs, todo=cc.locked), planPolicy, workflowVersion)
          case _ => System.err.println("Doing nothing")
        }
       }
      case "exec" | _ => {
        // for now, we'll hallucinate a fake version of what we're about to do
        // then, inside ExecuteMode, we might call create() for real,
        // but only if the user gives us the green light to go ahead with execution
        val uncommittedVersion = WorkflowVersionInfo.createFake()
        val planPolicy = getPlannedVertices(uncommittedVersion)

        val history = getVersionHistory()
        val cc = getCompletedTasks(planPolicy, history)
        val packageVersionThunk = { () => getPackageVersions(Some(cc), planPolicy, uncommittedVersion) }
        ExecuteMode.run(workflow, cc, planPolicy, history, uncommittedVersion, packageVersionThunk,traversal)
      }
    })
  }
}
