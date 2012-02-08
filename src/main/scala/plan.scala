package ducttape.workflow

import java.io._
import collection._

// TODO: Move filter here, but thoroughly describe the context
// in which it executes

object RealizationPlan {
  // read from file format for now, until we syntax-ify it
  def read(file: String): Seq[Map[BranchPoint, Set[Branch]]] = {
    val in = io.Source.fromFile(file)
    try {
      val list = new mutable.ArrayBuffer[Map[BranchPoint, Set[Branch]]]
      for(line <- in.getLines.toSeq) {
        val Seq(planName, goalTask, realInfo) = line.split(" : ").toSeq
        System.err.println("Ignoring plan name: " + planName)
        System.err.println("Ignoring goal task: " + goalTask)
        
        val mapping = {
          for(branchPointPlan <- realInfo.split("""\*""").toSeq.map(_.trim)) yield {
            assert(branchPointPlan.startsWith("("))
            assert(branchPointPlan.endsWith(")"))
            val inner = branchPointPlan.drop(1).dropRight(1)
            val toks = inner.split(" ").toSeq
            val branchPoint = BranchPoint(toks(0).dropRight(1))
            val branches = toks.drop(1).map(name => Branch(name, branchPoint))
            (branchPoint, branches.toSet)
          }
        }.toMap
        list += mapping
      }
      list
    } finally {
      in.close
    }
  }
}
