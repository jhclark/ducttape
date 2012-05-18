package ducttape.cli

import collection._
import ducttape.workflow.Realization
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.BranchPoint
import ducttape.workflow.Branch
import ducttape.workflow.RealTask
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.RealizationPlan
import ducttape.workflow.TaskTemplate
import ducttape.workflow.builder.WorkflowBuilder
import grizzled.slf4j.Logging

// TODO: Disconnect from CLI and move to workflow package
object Plans extends Logging {
  def getPlannedVertices(workflow: HyperWorkflow)
                        (implicit conf: Config): Set[(String,Realization)] = {  
    
      workflow.plans match {
        case Nil => {
          System.err.println("Using default one-off realization plan")
          Set.empty
        }
        case _ => {
          System.err.println("Finding hyperpaths contained in plan...")
          
          def getCandidates(branchFilter: Map[BranchPoint, Set[Branch]]) = {
            val numCores = 1
            // tasks only know about their parents in the form of (taskName, realization)
            // not as references to their realized tasks. this lets them get garbage collected
            // and reduces memory usage. however, we need all the candidate realized tasks on hand
            // (pre-filtered by realization, but not by goal vertex) so that we can make
            // a backward pass over the unpacked DAG
            val candidates = new mutable.HashMap[(String,Realization), RealTask]
      
            // this is the most important place for us to pass the filter to unpackedWalker!
            workflow.unpackedWalker(branchFilter).foreach(numCores, { v: UnpackedWorkVert => {
              val taskT: TaskTemplate = v.packed.value.get
              val task: RealTask = taskT.realize(v)
              candidates += (task.name, task.realization) -> task
            }})
            candidates
          }
          
          val vertexFilter = new mutable.HashSet[(String,Realization)]
          for (plan: RealizationPlan <- workflow.plans) {
            System.err.println("Finding vertices for plan: %s".format(plan.name))
            
            val candidates = getCandidates(plan.realizations)
            val fronteir = new mutable.Queue[RealTask]
            
            // initialize with all valid realizations of the goal vertex
            // (realizations have already been filtered during HyperDAG traversal)
            for (goalTask <- plan.goalTasks) {
              val goalRealTasks: Iterable[RealTask] = candidates.filter {
                case ( (tName, _), _) => tName == goalTask
              } map { _._2 }
              System.err.println("Found %d realizations of goal task %s: %s".
                format(goalRealTasks.size, goalTask, goalRealTasks.map{_.realization}.mkString(" ")))
              fronteir ++= goalRealTasks
            }
            
            val seen = new mutable.HashSet[RealTask]
            while (fronteir.size > 0) {
              val task: RealTask = fronteir.dequeue
              debug("Tracing back from task " + task)
              // add all parents (aka antecedents) to frontier
              if (!seen(task)) {
                try {
                  val antTasks: Set[RealTask] = task.antecedents.
                    map { case (taskName, real) => candidates(taskName, real) }
                  fronteir ++= antTasks
                } catch {
                  case e: NoSuchElementException => {
                    throw new RuntimeException("Error while trying to find antecedent tasks of %s".format(task), e)
                  }
                }
              }
              // mark this task as seen
              seen += task
            }
            
            // everything we saw is required to execute this realization plan to its goal vertices
            System.err.println("Found %d vertices implied by realization plan %s".format(seen.size, plan.name))
            vertexFilter ++= seen.map{task => (task.name, task.realization)}
          }
          System.err.println("Union of all planned vertices has size %d".format(vertexFilter.size))
          if (vertexFilter.isEmpty) {
            System.err.println("%sERROR: Plan includes zero tasks%s".format(conf.errorColor, conf.resetColor))
            System.exit(1)
          }
          vertexFilter
        }
      }
  }
}