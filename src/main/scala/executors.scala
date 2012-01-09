package ducttape

import System._
import collection._

import java.io.File

import ducttape.hyperdag._
import ducttape.Types._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.workflow._
import ducttape.util._

class DirectoryArchitect(val baseDir: File) {

  def assignDir(taskDef: TaskDef, realization: Map[String,Branch]): File = {
    val packedDir = assignPackedDir(taskDef)
    new File(packedDir, "%s/1".format(Task.realizationName(realization)))
  }

  def assignPackedDir(taskDef: TaskDef): File = {
    new File(baseDir, taskDef.name).getAbsoluteFile
  }

  def assignOutFile(spec: Spec, taskDef: TaskDef, realization: Map[String,Branch]): File = {
    println("Assigning outfile for " + spec)
    val taskDir = assignDir(taskDef, realization)
    assert(!spec.isInstanceOf[BranchPointDef])

    spec.rval match {
      case Unbound()  => { // user didn't specify a name for this output file
        new File(taskDir, spec.name) // will never collide with stdout.txt since it can't contain dots
      }
      case Literal(filename) => { // the user told us what name to use for the file -- put it under work/
        new File(taskDir, "work/%s".format(filename))
      }
    }
  }

  def isAbsolute(path: String) = new File(path).isAbsolute

  def getInFile(mySpec: Spec,
                realization: Map[String,Branch],
                srcSpec: Spec,
                srcTaskDef: TaskDef,
                srcRealization: Map[String,Branch]): File = {

    // first, resolve the realization, if necessary
    // TODO: Move this into the realization unpacking code
    val realizedRval = mySpec.rval match {
      case BranchPointDef(_,_) => srcSpec.rval // the "source" internal to the branch declaration
      case _ => mySpec.rval
    }

    // TODO: We should have already checked that this file exists by now?
    realizedRval match {
      case Literal(path) => isAbsolute(path) match {
        case true => new File(path)
        case false => new File(baseDir, path) // resolve relative paths relative to the workflow file (baseDir)
      }
      // branches, variables, etc get matched on the src, which we already resolved
      case _ => assignOutFile(srcSpec, srcTaskDef, srcRealization)
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

class Executor(conf: Config, dirs: DirectoryArchitect) extends UnpackedDagVisitor {

  override def visit(task: RealTask) {
    val where = dirs.assignDir(task.taskDef, task.activeBranches)
    val stdoutFile = new File(where, "stdout.txt")
    val stderrFile = new File(where, "stderr.txt")
    val workDir = new File(where, "work")
    
    println("%sConsidering running: %s/%s%s".format(conf.taskColor, task.name, task.realizationName, Console.RESET))

    val env = new mutable.ListBuffer[(String,String)]

    // TODO: Move this inside getInFile?
    def getSourceActiveBranches(task: RealTask) = {
      // if this came from a branch point, its source vertex will have
      // no knowledge of that branch point
      //
      // TODO: Simply removing one branch will not work once we
      // introduce constraint DAGs that allow branch points to be reused
      // XXX: For reasonably complex HyperDAGs this also breaks when the visibility
      // of a branch point X is conditioned on the choice by another branch point Y
      // TODO: Move all of these sorts of unit tests from LoonyBin to ducttape
      // TODO: Then associate Specs with edge info to link parent realizations properly
      //       (need realization FOR EACH E, NOT HE, since some vertices may have no knowlege of peers' metaedges)
      /*
      s.rval match {
        case BranchPointDef(branchPointName, _) => task.activeBranches.filter(_ != branchPointName)
        case _ => task.activeBranches
      }
      */
      val newBranchPoints = task.taskT.branchPoints.map(_.name).toSet
      task.activeBranches.filter{ case (bpName: String, branch: Branch) => !newBranchPoints(bpName)}
    }

    // grab input paths -- how are these to be resolved?
    for( (inSpec, srcSpec, srcTaskDef) <- task.inputVals) {
      val srcActiveBranches = getSourceActiveBranches(task)
      val inFile = dirs.getInFile(inSpec, task.activeBranches, srcSpec, srcTaskDef, srcActiveBranches)
      println("For inSpec %s with srcSpec %s, got path: %s".format(inSpec,srcSpec,inFile))
      env.append( (inSpec.name, inFile.getAbsolutePath) )
    }
    
    // set param values (no need to know source active branches since we already resolved the literal)
    val params: Seq[(String,String)] = for( (paramSpec, srcSpec, srcTaskDef) <- task.paramVals) yield {
      println("For paramSpec %s with srcSpec %s, got value: %s".format(paramSpec,srcSpec,srcSpec.rval.value))
      env.append( (paramSpec.name, srcSpec.rval.value) )
      (paramSpec.name, srcSpec.rval.value)
    }

    // assign output paths
    var allOutFilesExist = true
    allOutFilesExist = stdoutFile.exists() && stderrFile.exists()
    for(outSpec <- task.outputs) {
      val outFile = dirs.assignOutFile(outSpec, task.taskDef, task.activeBranches)
      if(!outFile.exists)
        allOutFilesExist = false
      println("For outSpec %s got path: %s".format(outSpec, outFile))
      env.append( (outSpec.name, outFile.getAbsolutePath) )
    }

    // TODO: Move this check and make it check file size and date with fallback to checksums? or always checksums? or checksum only if files are under a certain size?
    if(allOutFilesExist) {
      err.println("Determined that %s already has all required outputs".format(task.name))
    } else {

      println("Running %s in %s".format(task.name, where.getAbsolutePath))
      if(where.exists) {
        err.println("Partial output detected at %s; DELETING ALL PARTIAL OUTPUT".format(where))
        Files.deleteDir(where)
      }
      workDir.mkdirs
      if(!workDir.exists) {
        throw new RuntimeException("Could not make directory: " + where.getAbsolutePath)
      }
      
      // TODO: Get the environment variables up in there! Was this already taken care of by -V?
      val commandsWithGimme = Gimme.augment(dirs.baseDir, params, task.commands)
      val submitCommands = Submitter.prepare(dirs.baseDir, where, params, commandsWithGimme)
      val exitCode = Shell.run(submitCommands, workDir, env, stdoutFile, stderrFile)
      if(exitCode != 0) {
        println("%sTask %s/%s returned %s%s".format(conf.errorColor, task.name, task.realizationName, exitCode, Console.RESET))
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
