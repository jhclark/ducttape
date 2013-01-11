package ducttape.workflow

import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.SpecTypes._

/**
 * The main class passed around to the workflow manager that contains all
 * information necessary to work with a task. It can be created via RealTask.toVersionedTask
 * 
 * a realized task with the addition of a workflow version number
 * together, these give us sufficient information to assign a directory path to the task */
class VersionedTask(realTask: RealTask,
                    val inputValVersions: Seq[VersionedSpec],
                    val version: Int)
  extends RealTask(realTask) {

  def toVersionedTaskId() = new VersionedTaskId(namespace, realization.toCanonicalString, version)

  // TODO: Smear hash code better
  override def hashCode() = name.hashCode ^ realization.hashCode
  override def equals(obj: Any) = obj match {
    case that: VersionedTask => this.name == that.name && this.realization == that.realization && this.version == that.version
  }

  // TODO: Specialize if we're flat
  override def toString() = "%s/%s/%d".format(name, realization.toString, version)
}


