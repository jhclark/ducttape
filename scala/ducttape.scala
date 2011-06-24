import ducttape.io._
import ducttape.hyperdag._
import ducttape.Types._
import ducttape.workflow._
import ducttape.util._
import System._

object Ducttape {
  def main(args: Array[String]) {
    println("%sDuctTape v0.1".format(Console.YELLOW))
    println("%sBy Jonathan Clark".format(Console.BLUE))
    println(Console.RESET)

    if(args.length != 1) {
      err.println("Usage: ducctape workflow.tape")
      exit(1)
    }

    var file = args(0)
    println("Reading workflow from %s".format(file))
    val wd: WorkflowDefinition = MakelikeDSL.read(file)
    println("Building workflow...")
    val workflow: HyperWorkflow = WorkflowBuilder.build(wd)
    println("Workflow contains %d tasks".format(workflow.dag.size))
    for(v: UnpackedWorkVert <- workflow.dag.unpackedWalker.iterator) {
      val task = v.packed.value
      println("Running %s".format(task.name))
      Shell.run(task.commands)
    }
  }
}
