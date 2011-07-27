//import ducttape.io._
import ducttape.hyperdag._
import ducttape.Types._
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.syntax.GrammarParser
import ducttape.workflow._
import ducttape.util._
import System._

import java.io.File
class DirectoryArchitect(val baseDir: File) {
  def assignDir(task: TaskDef) = {
    new File(baseDir, "%s/baseline/1".format(task.name))
  }

  // TODO: Allow user to specify filename -- in which case we put it in work/ dir
  def assignOutFile(spec: Spec, taskDef: TaskDef): File = {
    new File(assignDir(taskDef), spec.name)
  }

  def getInFile(spec: Spec, srcTaskDef: TaskDef): File = {
    spec.rval match {
      case Literal(value) => {
        // TODO: Determine if this is a relative or absolute path
        // and define behavior
        // TODO: We should have already checked that this file exists by now
        return new File(value)
      }
      case Unbound() => {
        return assignOutFile(spec, srcTaskDef)
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
    val wd: WorkflowDefinition = GrammarParser.read(file)
    println("Building workflow...")
    val workflow: HyperWorkflow = WorkflowBuilder.build(wd)
    println("Workflow contains %d tasks".format(workflow.dag.size))

    val baseDir = file.getParentFile
    val dirs = new DirectoryArchitect(baseDir)
    for(v: UnpackedWorkVert <- workflow.dag.unpackedWalker.iterator) {
      val task = v.packed.value

      val where = dirs.assignDir(task.taskDef)
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
        val inFile = dirs.getInFile(srcSpec, srcTaskDef)
        println("For inSpec %s with srcSpec %s, got path: %s".format(inSpec,srcSpec,inFile))
        env.append( (inSpec.name, inFile.getAbsolutePath) )
      }
      
      // set param values
      for( (paramSpec, srcSpec, srcTaskDef) <- task.paramVals) {
        env.append( (paramSpec.name, srcSpec.rval.value) )
      }

      // assign output paths
      for(outSpec <- task.outputs) {
        val outFile = dirs.assignOutFile(outSpec, task.taskDef)
        env.append( (outSpec.name, outFile.getAbsolutePath) )
      }
      Shell.run(task.commands, workDir, env, stdoutFile, stderrFile)
    }
  }
}
