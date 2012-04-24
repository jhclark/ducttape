package ducttape.cli

import collection.Set
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.RealTask
import ducttape.exec.FullTaskEnvironment
import ducttape.workflow.TaskTemplate
import ducttape.exec.DirectoryArchitect
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.Realization
import ducttape.exec.PackageVersioner

object EnvironmentMode {
  def run(workflow: HyperWorkflow,
          plannedVertices: Set[(String,Realization)],
          getPackageVersions: () => PackageVersioner)
         (implicit opts: Opts, conf: Config, dirs: DirectoryArchitect) {
    if (opts.taskName == None) {
      opts.exitHelp("env requires a taskName", 1)
    }
    if(opts.realNames.size != 1) {
      opts.exitHelp("env requires one realization name", 1)
    }
    val goalTaskName = opts.taskName.get
    val goalRealName = opts.realNames.head

    // TODO: Dont' apply plan filter?
    // TODO: Apply filters so that we do much less work to get here
    var matchingTasks: Iterable[UnpackedWorkVert] = {
      workflow.unpackedWalker(plannedVertices=plannedVertices).iterator.filter{v: UnpackedWorkVert => v.packed.value.name == goalTaskName}
    }.toIterable
    System.err.println("Found %d vertices with matching task name".format(matchingTasks.size))
    
    var matchingReals: Iterable[RealTask] = {
      matchingTasks.map{v: UnpackedWorkVert => {
        val taskT: TaskTemplate = v.packed.value
        val task: RealTask = taskT.realize(v)
        if (task.realization.toString == goalRealName) {
          System.err.println("My parents are: " + v.parentRealizations)
          Some(task)
        } else {
          None
        }
      }}.filter(_ != None).map(_.get)
    }
    System.err.println("Found %d vertices with matching realizations".format(matchingReals.size))
    
    val packageVersions = getPackageVersions()
    
    for (task: RealTask <- matchingReals) {
      val env = new FullTaskEnvironment(dirs, packageVersions, task)
      for( (k,v) <- env.env) {
        println("%s=%s".format(k,v))
      }
    }
  }
}