package ducttape.versioner

import java.io.File

import ducttape.exec.DirectoryArchitect
import ducttape.exec.PackageVersioner

import ducttape.hyperdag.PackedVertex
import ducttape.workflow.RealTaskId
import ducttape.workflow.VersionedTaskId
import ducttape.workflow.TaskTemplate
import ducttape.workflow.HyperWorkflow

/** concrete implementation is WorkflowVersionStore */
// TODO: Add date information
trait WorkflowVersionInfo {
  val version: Int
  def existing: Seq[VersionedTaskId]
  def todo: Seq[VersionedTaskId]
  def packages: Seq[VersionedPackageId]
  def packageDeps: Seq[(VersionedTaskId,VersionedPackageId)]

  def apply(task: RealTaskId): Int
  def get(task: RealTaskId): Option[Int]
}

object FakeWorkflowVersionInfo extends WorkflowVersionInfo {
  val FAKE_VERSION: Int = 0
  val version: Int = FAKE_VERSION
  def existing: Seq[VersionedTaskId] = Seq.empty
  def todo: Seq[VersionedTaskId] = Seq.empty
  def packages: Seq[VersionedPackageId] = Seq.empty
  def packageDeps: Seq[(VersionedTaskId,VersionedPackageId)] = Seq.empty

  def apply(task: RealTaskId): Int = FAKE_VERSION
  def get(task: RealTaskId): Option[Int] = Some(FAKE_VERSION)
}

/** allows us to carry on, even in the presence of corrupt workflow versions */
class CorruptWorkflowVersionInfo(val version: Int) extends WorkflowVersionInfo {
  def existing: Seq[VersionedTaskId] = Nil
  def todo: Seq[VersionedTaskId] = Nil
  def packages: Seq[VersionedPackageId] = Seq.empty
  def packageDeps: Seq[(VersionedTaskId,VersionedPackageId)] = Seq.empty

  def apply(task: RealTaskId): Int = throw new RuntimeException("apply() is unsupported for CorruptWorkflowVersionInfo")
  def get(task: RealTaskId): Option[Int] = None
}

/** A temporary place-holder for a workflow version that's
 *  about to be created iff the user gives us the okay.
 *  Once the user gives us the okay, we use the commit() method
 *  to write it to disk and create a WorkflowVersionStore.
 * 
 *  TODO: We should lock this directory on disk while the user
 *        decides whether or not to proceed */
class TentativeWorkflowVersionInfo(dirs: DirectoryArchitect,
                                   workflow: HyperWorkflow,
                                   history: WorkflowVersionHistory,
                                   packageVersioner: PackageVersioner,
                                   existingTasks: Seq[VersionedTaskId],
                                   todoTasks: Seq[VersionedTaskId]) extends WorkflowVersionInfo {

  lazy val taskVersions: Map[RealTaskId,Int] = WorkflowVersionStore.toMap(existingTasks, todoTasks)

  // TODO: Check for namespace issues
  private lazy val packageVersionInfo: Seq[VersionedPackageId] = packageVersioner.packageVersions.toSeq.map {
    case (namespace, ver) => new VersionedPackageId(namespace.name, ver)
  }
  private lazy val _packageDeps: Seq[(VersionedTaskId,VersionedPackageId)] = {
    // TODO: Check for namespace issues
    val packageMap: Map[String,VersionedPackageId] = { packages.map { p => (p.packageName, p) } }.toMap

    workflow.packedWalker.iterator.toSeq.flatMap { v: PackedVertex[Option[TaskTemplate]] =>
      val taskT: TaskTemplate = v.value.get
      val packageNames: Seq[String] = taskT.packages.map(_.name)
      packageNames.flatMap { name: String =>
        val packageVer: VersionedPackageId = packageMap(name)
        // loop over all versions of this task
        todoTasks.filter { _.name == taskT.name } map { taskVer: VersionedTaskId =>
          (taskVer, packageVer)
        }
      }
    }
  }

  val version: Int = history.nextVersion
  def existing: Seq[VersionedTaskId] = existingTasks
  def todo: Seq[VersionedTaskId] = todoTasks
  def packages: Seq[VersionedPackageId] = packageVersionInfo
  def packageDeps: Seq[(VersionedTaskId,VersionedPackageId)] = _packageDeps

  override def apply(task: RealTaskId): Int = taskVersions(task)
  override def get(task: RealTaskId): Option[Int] = taskVersions.get(task)

  def commit(): WorkflowVersionStore
    = WorkflowVersionStore.create(dirs, workflow, history, packages, packageDeps, existing, todoTasks)
}

/** fallbackVersion is returned by apply() if there is no existing version for the task
 *  get() will return None if there is no existing version for a task */
class UnionWorkflowVersionInfo(val version: Int,
                               val existing: Seq[VersionedTaskId],
                               val todo: Seq[VersionedTaskId],
                               val fallbackVersion: Int) extends WorkflowVersionInfo {
  val taskMap: Map[RealTaskId,Int] = {
    existing.map { task: VersionedTaskId => (task.toRealTaskId, task.version) }
  }.toMap

  def packages: Seq[VersionedPackageId] = throw new RuntimeException("Not implemented")
  def packageDeps: Seq[(VersionedTaskId,VersionedPackageId)] = throw new RuntimeException("Not implemented")

  def apply(task: RealTaskId): Int = taskMap.getOrElse(task, fallbackVersion)
  def get(task: RealTaskId): Option[Int] = taskMap.get(task)
}

object WorkflowVersionInfo {
  // hallucinate a new version without actually committing a new version to disk
  def createFake(): WorkflowVersionInfo = FakeWorkflowVersionInfo
}
