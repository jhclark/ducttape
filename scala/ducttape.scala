import ducttape.io._
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
    val tasks: Seq[Task] = MakelikeDSL.read(file)
    // TODO: Convert the raw tasks into a DAG
    for(task <- tasks) {
      println("Running %s".format(task.name))
      Shell.run(task.commands)
    }
  }
}
