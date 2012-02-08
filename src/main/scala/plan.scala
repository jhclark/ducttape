package ducttape.workflow

import java.io._

// TODO: Move filter here, but thoroughly describe the context
// in which it executes

object RealizationPlan {
  // read from file format for now, until we syntax-ify it
  def read(file: String): Seq[Map[BranchPoint, Set[Branch]]] = {
    val in = io.Source.fromFile(file)
    try {
      for(line <- in.getLines) {
        val Seq(planName, goalTask, realInfo) = line.split(" : ").toSeq
        println(planName)
        println(goalTask)
        println(realInfo)
      }
      Nil
    } finally {
      in.close
    }
  }
}
