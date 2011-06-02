import ducttape.io._
import ducttape.util._

object Ducttape {
  def main(args: Array[String]) {
    println("%sDuctTape v0.1".format(Console.YELLOW))
    println("%sBy Jonathan Clark".format(Console.BLUE))
    println(Console.RESET)

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
