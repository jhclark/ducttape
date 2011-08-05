import System._
import collection._

import java.io.File

import ducttape.hyperdag._
import ducttape.Types._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.GrammarParser
import ducttape.workflow._
import ducttape.util._

class DirectoryArchitect(val baseDir: File) {
  def assignDir(taskDef: TaskDef, realization: Map[String,Branch]) = {
    val packedDir = assignPackedDir(taskDef)
    new File(packedDir, "%s/1".format(Task.realizationName(realization)))
  }

  def assignPackedDir(taskDef: TaskDef) = {
    new File(baseDir, taskDef.name)
  }

  // TODO: Allow user to specify filename -- in which case we put it in work/ dir
  def assignOutFile(spec: Spec, taskDef: TaskDef, realization: Map[String,Branch]): File = {
    val dir = assignDir(taskDef, realization)
    new File(dir, spec.name)
  }

  def isAbsolute(path: String) = new File(path).isAbsolute

  def getInFile(spec: Spec, srcTaskDef: TaskDef, srcRealization: Map[String,Branch]): File = {
    // TODO: We should have already checked that this file exists by now?
    spec.rval match {
      case Literal(value) => {
        if(isAbsolute(value)) {
          return new File(value)
        } else {
          // resolve relative paths relative to the workflow file (baseDir)
          return new File(baseDir, value)
        }
      }
      case Unbound() => {
        return assignOutFile(spec, srcTaskDef, srcRealization)
      }
    }
  }
}

// TODO: Abstract beyond just workflows?
trait PackedDagVisitor {
  def visit(task: TaskTemplate)
}

trait UnpackedDagVisitor {
  def visit(task: RealTask)
}

class Config {
  var headerColor = Console.BLUE
  var byColor = Console.BLUE
  var taskColor = Console.GREEN
  var errorColor = Console.RED
}

class Executor(conf: Config, dirs: DirectoryArchitect) extends UnpackedDagVisitor {
  override def visit(task: RealTask) {
    val where = dirs.assignDir(task.taskDef, task.activeBranches)
    val stdoutFile = new File(where, "stdout.txt")
    val stderrFile = new File(where, "stderr.txt")
    val workDir = new File(where, "work")
    
    println("%sRunning: %s/%s%s".format(conf.taskColor, task.name, task.realizationName, Console.RESET))
    println("Running %s in %s".format(task.name, where.getAbsolutePath))
    if(where.exists) {
      println("Partial output detected at %s; NOT DELETING".format(where))
      //Files.deleteDir(where)
    }
    workDir.mkdirs
    if(!workDir.exists) {
      throw new RuntimeException("Could not make directory: " + where.getAbsolutePath)
    }
      
    val env = new mutable.ListBuffer[(String,String)]

    // grab input paths -- how are these to be resolved?
    for( (inSpec, srcSpec, srcTaskDef) <- task.inputVals) {

      // TODO: Move this inside getInFile?
      val srcActiveBranches = inSpec.rval match {
        // if this came from a branch point, it's source vertex will have
        // no knowledge of that branch point
        //
        // TODO: Simply removing one branch will not work once we
        // introduce constraint DAGs that allow branch points to be reused
        case BranchPointDef(branchPointName, _)
        => task.activeBranches.filter(_ != branchPointName)
        case _ => task.activeBranches
      }
      
      val inFile = dirs.getInFile(srcSpec, srcTaskDef, srcActiveBranches)
      println("For inSpec %s with srcSpec %s, got path: %s".format(inSpec,srcSpec,inFile))
      env.append( (inSpec.name, inFile.getAbsolutePath) )
    }
    
    // set param values
    // TODO: Make these work properly with hyperworkflows
    for( (paramSpec, srcSpec, srcTaskDef) <- task.paramVals) {
      env.append( (paramSpec.name, srcSpec.rval.value) )
    }

    // assign output paths
    var allOutFilesExist = true
    for(outSpec <- task.outputs) {
      val outFile = dirs.assignOutFile(outSpec, task.taskDef, task.activeBranches)
      if(!outFile.exists)
        allOutFilesExist = false
      env.append( (outSpec.name, outFile.getAbsolutePath) )
    }

    // TODO: Move this check and make it into checksums
    if(allOutFilesExist) {
      err.println("Determined that %s already has all required outputs".format(task.name))
    } else {
      val code = Shell.run(task.commands, workDir, env, stdoutFile, stderrFile)
      if(code != 0) {
        println("%sTask %s/%s returned %s%s".format(conf.errorColor, task.name, task.realizationName, code, Console.RESET))
        exit(1)
      }
    }
  }
}

class Purger(conf: Config, dirs: DirectoryArchitect) extends PackedDagVisitor {
  override def visit(task: TaskTemplate) {
    val where = dirs.assignPackedDir(task.taskDef)
    println("Removing directory: %s".format(where.getAbsolutePath))
    if(where.exists) {
      Files.deleteDir(where)
    }
  }
}

object Ducttape {
  def main(args: Array[String]) {
    val conf = new Config

    println("%sDuctTape v0.1".format(conf.headerColor))
    println("%sBy Jonathan Clark".format(conf.byColor))
    println(Console.RESET)

    if(args.length == 0) {
      err.println("Usage: ducctape workflow.tape [--purge]")
      exit(1)
    }

    // TODO: Confirm this drastic action
    val purge = {args.length > 1 && args(1) == "--purge"}

    var file = new File(args(0))
    println("Reading workflow from %s".format(file.getAbsolutePath))
    val wd: WorkflowDefinition = GrammarParser.read(IO.read(file, "UTF-8"))
    println("Building workflow...")
    val workflow: HyperWorkflow = WorkflowBuilder.build(wd)
    println("Workflow contains %d tasks".format(workflow.dag.size))
    
    // TODO: Check that all input files exist

    println("Executing...")

    val baseDir = file.getParentFile
    val dirs = new DirectoryArchitect(baseDir)

    purge match {
      case true => {
        val visitor: PackedDagVisitor = new Purger(conf, dirs)
        for(v: PackedWorkVert <- workflow.dag.packedWalker.iterator) {
          visitor.visit(v.value)
        }
      }
      case false => {
        val visitor: UnpackedDagVisitor = new Executor(conf, dirs)
        for(v: UnpackedWorkVert <- workflow.dag.unpackedWalker.iterator) {
          val taskT: TaskTemplate = v.packed.value
          val task: RealTask = taskT.realize(v.realization)
          visitor.visit(task)
        }
      }
    }
  }
}
