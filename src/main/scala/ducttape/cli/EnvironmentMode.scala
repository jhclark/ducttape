// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.cli

import collection.Set
import ducttape.exec.FullTaskEnvironment
import ducttape.exec.DirectoryArchitect
import ducttape.exec.PackageVersioner
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.RealTask
import ducttape.workflow.VersionedTask
import ducttape.workflow.PlanPolicy
import ducttape.workflow.TaskTemplate
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.Realization
import ducttape.versioner.WorkflowVersionInfo
import ducttape.util.Shell

object EnvironmentMode {
  
  def run(workflow: HyperWorkflow,
          planPolicy: PlanPolicy,
          packageVersions: PackageVersioner,
          workflowVersion: WorkflowVersionInfo, // this should be from some *previous* version
          showCommands: Boolean = false)
         (implicit opts: Opts, dirs: DirectoryArchitect) {
    
    if (opts.taskName == None) {
      opts.exitHelp("env/commands requires a taskName", 1)
    }
    if (opts.realNames.size != 1) {
      opts.exitHelp("env/commands requires one realization name", 1)
    }
    val goalTaskName = opts.taskName.get
    val goalRealName = opts.realNames.head

    // TODO: Dont' apply plan filter?
    // TODO: Apply filters so that we do much less work to get here
    val matchingTasks: Iterable[UnpackedWorkVert] = {
      workflow.unpackedWalker(planPolicy).iterator.
        filter { v: UnpackedWorkVert => goalTaskName == "*" || v.packed.value.get.name == goalTaskName }
    }.toIterable
    System.err.println("Found %d vertices with matching task name".format(matchingTasks.size))
    
    val matchingReals: Iterable[VersionedTask] = {
      matchingTasks.map { v: UnpackedWorkVert =>
        val taskT: TaskTemplate = v.packed.value.get
        val task: VersionedTask = taskT.toRealTask(v).toVersionedTask(workflowVersion)
        if (goalRealName == "*" || task.realization.toString == goalRealName) {
          Some(task)
        } else {
          None
        }
      }.filter(_ != None).map(_.get)
    }
    System.err.println("Found %d vertices with matching realizations".format(matchingReals.size))
    
    for (task: VersionedTask <- matchingReals) {
      println("# " + task.name + " " + task.realization + ":")
      val env = new FullTaskEnvironment(dirs, packageVersions, task)
      for ( (k,v) <- env.env) {
        if (showCommands) {
          println("export %s=%s".format(k,v))
        } else {
          println("%s=%s".format(k,v))
        }
      }
      if (showCommands) {
        println(Shell.BASH_FLAGS.mkString("\n"))
        println(task.commands.code)
      }
      println()
    }
  }
}
