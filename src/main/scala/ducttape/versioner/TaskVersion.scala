package ducttape.versioner

import collection._

// TODO: Diff the workflow files themselves
trait TaskVersion {
  def ==(that: TaskVersion) = this.digest() == that.digest()
  def digest(): Int;
  def toString(): String;
}
/*
// do config files first because...
// This must also include modifications specified in external config files!!!
class TaskVersion(val task: TaskDef,
                  val taskVer: Version,
                  val inVers: Seq[(Spec,Version)]) extends Version {

  def digest() = {
    // how do we factor out choice of digest algorithm? config allows different ones for workflow, files, aggregation, etc?
  }

  def print(out: PrintStream) = {
    out.println("%s = %s".format(task.name, taskVer.toString))
    for( (inFileSpec, v) <- inVers) {
      out.println("%s = %s".format(inFileSpec.name, v.toString))
    }
  }
}

class WorkflowVersion(val when: Date) extends Version {
  // contains a version hash or revision (Integer? Special DataType?)
  // for every taskdef, every input file, (output file?), 
  override def toString() = {
    ""
  }
}

// TODO: Use java.security.DigestInputStream?


class WorkflowVersioner {

  val versions = List[WorkflowVersion]

  def toString() = {
    ""
  }
}
*/
