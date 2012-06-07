package ducttape.cli

import collection._

import java.util.concurrent.ExecutionException
import java.io.File

import ducttape.syntax.FileFormatException
import ducttape.exec.PackageBuilder
import ducttape.versioner.WorkflowVersionInfo
import ducttape.exec.PackageVersioner
import ducttape.exec.InputChecker
import ducttape.exec.DirectoryArchitect
import ducttape.exec.CompletionChecker
import ducttape.exec.PartialOutputMover
import ducttape.exec.Executor
import ducttape.exec.PidWriter
import ducttape.exec.LockManager
import ducttape.workflow.Visitors
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.Realization
import ducttape.versioner.WorkflowVersionHistory
import ducttape.util.Files

object ExecuteMode {
  
  def run(workflow: HyperWorkflow,
          cc: CompletionChecker,
          plannedVertices: Set[(String, Realization)],
          history: WorkflowVersionHistory,
          getPackageVersions: () => PackageVersioner)
         (implicit opts: Opts, conf: Config, dirs: DirectoryArchitect) {
    
    if (cc.todo.isEmpty) {
      // TODO: Might need to re-run if any package versions have changed
      System.err.println("All tasks to complete -- nothing to do")
    } else {
      System.err.println("Finding packages...")
      val packageVersions = getPackageVersions()
      
      System.err.println("Checking inputs...")
      val inputChecker = new InputChecker(dirs)
      Visitors.visitAll(workflow, inputChecker, plannedVertices)
      if (inputChecker.errors.size > 0) {
        for (e: FileFormatException <- inputChecker.errors) {
          ErrorUtils.prettyPrintError(e, prefix="ERROR", color=conf.errorColor)
        }
        System.err.println("%d errors".format(inputChecker.errors.size))
        System.exit(1)
      }
      
      // TODO: Check package versions to see if any packages need rebuilding.
  
      // TODO: Check for existing PID lock files from some other process...x
      
      import ducttape.cli.ColorUtils.colorizeDir
      import ducttape.cli.ColorUtils.colorizeDirs
      
      System.err.println("Work plan:")
      for ( (task, real) <- cc.broken) {
        System.err.println("%sDELETE:%s %s".format(conf.redColor, conf.resetColor, colorizeDir(task, real)))
      }
      for ( (task, real) <- cc.partial) {
        System.err.println("%sMOVE TO ATTIC:%s %s".format(conf.redColor, conf.resetColor, colorizeDir(task, real)))
      }
      for (packageName <- packageVersions.packagesToBuild) {
        System.err.println("%sBUILD:%s %s".format(conf.greenColor, conf.resetColor, packageName))
      }
      for ( (task, real) <- cc.locked) {
        System.err.println("%sWAIT FOR LOCK:%s %s".format(conf.greenColor, conf.resetColor, colorizeDir(task, real)))
      }
      for ( (task, real) <- cc.todo) {
        System.err.println("%sRUN:%s %s".format(conf.greenColor, conf.resetColor, colorizeDir(task, real)))
      }
  
      val answer = if (opts.yes) {
        true
      } else {
        // note: user must still press enter
        if (cc.partial.size > 0) {
          System.err.print("Are you sure you want to MOVE all this partial output to the attic and then run these %d tasks? [y/n] ".format(cc.todo.size))
        } else {
          System.err.print("Are you sure you want to run these %d tasks? [y/n] ".format(cc.todo.size))
        }
        Console.readBoolean
      }
      
      answer match {
        case true => {
          // create a new workflow version
          val configFile: Option[File] = opts.config_file.value.map(new File(_))
          val myVersion: WorkflowVersionInfo = WorkflowVersionInfo.create(dirs, opts.workflowFile, configFile, history)
          
          // before doing *anything* else, make sure our output directory exists, so that we can lock things
          Files.mkdirs(dirs.confBaseDir)
          
          System.err.println("Retreiving code and building...")
          val builder = new PackageBuilder(dirs, packageVersions)
          builder.build(packageVersions.packagesToBuild)
  
          val locker = new LockManager(myVersion)
          
          System.err.println("Moving previous partial output to the attic...")
          // NOTE: We get the version of each failed attempt from its version file (partial). Lacking that, we kill it (broken).
          Visitors.visitAll(workflow, new PartialOutputMover(dirs, cc.partial, cc.broken, locker), plannedVertices)
          
          // Make a pass after moving partial output to write output files
          // claiming those directories as ours so that we can later start another ducttape process
          Visitors.visitAll(workflow, new PidWriter(dirs, myVersion, cc.todo, locker), plannedVertices)
          System.err.println("Executing tasks...")
          try {
            Visitors.visitAll(workflow,
                              new Executor(dirs, packageVersions, locker, workflow, plannedVertices, cc.completed, cc.todo),
                              plannedVertices, opts.jobs())
          } finally {
            // release all of our locks, even if we go down in flames
            Visitors.visitAll(workflow, new PidWriter(dirs, myVersion, cc.todo, locker, remove=true), plannedVertices)
          }
        }
        case _ => System.err.println("Doing nothing")
      }
    }
  }
}