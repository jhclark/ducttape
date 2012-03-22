package ducttape.workflow

import collection._

// TODO: Move filter here, but thoroughly describe the context
// in which it executes

object RealizationPlan {
  // read from file format for now, until we syntax-ify it
  def read(file: String, branchPointFactory: BranchPointFactory, branchFactory: BranchFactory): Seq[RealizationPlan] = {
    val in = io.Source.fromFile(file)
    try {
      val list = new mutable.ArrayBuffer[RealizationPlan]
      for(line <- in.getLines.toSeq) {
        val Seq(planName, goalTask, realInfo) = line.split(" : ").toSeq
        
        val mapping = {
          for(branchPointPlan <- realInfo.split("""\*""").toSeq.map(_.trim)) yield {
            assert(branchPointPlan.startsWith("("))
            assert(branchPointPlan.endsWith(")"))
            val inner = branchPointPlan.drop(1).dropRight(1)
            val toks = inner.split(" ").toSeq
            try {
              val branchPoint = branchPointFactory(toks(0).dropRight(1))
              val branches = toks.drop(1).map(name => branchFactory(name, branchPoint))
              (branchPoint, branches.toSet)
            } catch {
              // TODO: Move to err2exception
              case e: NoSuchBranchPointException => {
                Console.err.println("ERROR: No such branch point: %s".format(e.msg))
                sys.exit(1)
              }
              case e: NoSuchBranchException => {
                Console.err.println("ERROR: No such branch: %s".format(e.msg))
                sys.exit(1)
              }
            }
          }
        }.toMap
        list += new RealizationPlan(planName, Seq(goalTask), mapping)
      }
      list
    } finally {
      in.close
    }
  }
}

class RealizationPlan(val name: String, val goalTasks: Seq[String], val realizations: Map[BranchPoint, Set[Branch]]);
