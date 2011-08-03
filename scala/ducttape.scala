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
    new File(baseDir, "%s/%s/1".format(taskDef.name, Task.realizationName(realization)))
  }

  // TODO: Allow user to specify filename -- in which case we put it in work/ dir
  def assignOutFile(spec: Spec, taskDef: TaskDef, realization: Map[String,Branch]): File = {
    val dir = assignDir(taskDef, realization)
    new File(dir, spec.name)
  }

  def getInFile(spec: Spec, srcTaskDef: TaskDef, srcRealization: Map[String,Branch]): File = {
    spec.rval match {
      case Literal(value) => {
        // TODO: Determine if this is a relative or absolute path
        // and define behavior
        // TODO: We should have already checked that this file exists by now
        return new File(value)
      }
      case Unbound() => {
        return assignOutFile(spec, srcTaskDef, srcRealization)
      }
    }
  }
}

object Ducttape {
  def main(args: Array[String]) {
    println("%sDuctTape v0.1".format(Console.YELLOW))
    println("%sBy Jonathan Clark".format(Console.BLUE))
    println(Console.RESET)

    if(args.length != 1) {
      err.println("Usage: ducctape workflow.tape")
      exit(1)
    }

    var file = new File(args(0))
    println("Reading workflow from %s".format(file.getAbsolutePath))
    val wd: WorkflowDefinition = GrammarParser.read(IO.read(file, "UTF-8"))
    println("Building workflow...")
    val workflow: HyperWorkflow = WorkflowBuilder.build(wd)
    println("Workflow contains %d tasks".format(workflow.dag.size))
    println("Executing...")

    val baseDir = file.getParentFile
    val dirs = new DirectoryArchitect(baseDir)
    for(v: UnpackedWorkVert <- workflow.dag.unpackedWalker.iterator) {
      val taskT: TaskTemplate  = v.packed.value
      val task: RealTask = taskT.realize(v.realization)
      println("TASK: %s Realization: %s".format(task.name, task.realizationName))

      val where = dirs.assignDir(task.taskDef, task.activeBranches)
      val stdoutFile = new File(where, "stdout.txt")
      val stderrFile = new File(where, "stderr.txt")
      val workDir = new File(where, "work")

      println("Running %s in %s".format(task.name, where.getAbsolutePath))
      if(where.exists) {
        println("Removing partial output at %s".format(where))
        where.delete
      }
      workDir.mkdirs
      if(!workDir.exists) {
        throw new RuntimeException("Could not make directory: " + where.getAbsolutePath)
      }
      
      import collection._
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
      for( (paramSpec, srcSpec, srcTaskDef) <- task.paramVals) {
        env.append( (paramSpec.name, srcSpec.rval.value) )
      }

      // assign output paths
      for(outSpec <- task.outputs) {
        val outFile = dirs.assignOutFile(outSpec, task.taskDef, task.activeBranches)
        env.append( (outSpec.name, outFile.getAbsolutePath) )
      }
      Shell.run(task.commands, workDir, env, stdoutFile, stderrFile)
    }
  }
}
