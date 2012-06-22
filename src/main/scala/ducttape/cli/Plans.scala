package ducttape.cli

import collection._
import ducttape.syntax.FileFormatException
import ducttape.workflow.Realization
import ducttape.workflow.HyperWorkflow
import ducttape.workflow.BranchPoint
import ducttape.workflow.Branch
import ducttape.workflow.PlanPolicy
import ducttape.workflow.OneOff
import ducttape.workflow.PatternFilter
import ducttape.workflow.VertexFilter
import ducttape.workflow.RealTask
import ducttape.workflow.SpecGroup
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.Types.PackedWorkVert
import ducttape.workflow.Types.WorkflowMetaEdge
import ducttape.workflow.Types.WorkflowEdge
import ducttape.workflow.RealizationPlan
import ducttape.workflow.TaskTemplate
import ducttape.workflow.builder.WorkflowBuilder
import ducttape.util.Optional

import grizzled.slf4j.Logging

// TODO: Disconnect from CLI and move to workflow package
object Plans extends Logging {
  
  // implemented the second forward pass through the HyperDAG
  // using a PatternFilter
  def getCandidates(workflow: HyperWorkflow,
                    plan: RealizationPlan,
                    explainCallback: (Option[String], =>String, =>String, Boolean) => Unit,
                    graftRelaxations: Map[PackedWorkVert, Set[Branch]])
      : Map[(String,Realization), RealTask] = {
    val numCores = 1
    // tasks only know about their parents in the form of (taskName, realization)
    // not as references to their realized tasks. this lets them get garbage collected
    // and reduces memory usage. however, we need all the candidate realized tasks on hand
    // (pre-filtered by realization, but not by goal vertex) so that we can make
    // a backward pass over the unpacked DAG
    val candidates = new mutable.HashMap[(String,Realization), RealTask]

    def explainCallbackCurried(vertexName: => String, msg: => String, accepted: Boolean)
      = explainCallback(plan.name, vertexName, msg, accepted)

    // this is the most important place for us to pass the filter to unpackedWalker!
    workflow.unpackedWalker(PatternFilter(plan.realizations, graftRelaxations), explainCallbackCurried).
      foreach(numCores, { v: UnpackedWorkVert =>
        val taskT: TaskTemplate = v.packed.value.get
        val task: RealTask = taskT.realize(v)
        candidates += (task.name, task.realization) -> task
    })
    candidates
  }
  
  // TODO: This is overzealous since these are not necessarily
  //   all dependencies in the unpacked DAG
  def getDependencies(workflow: HyperWorkflow, v: PackedWorkVert, includeV: Boolean): Seq[PackedWorkVert] = {
    (if (includeV) Seq(v) else Nil) ++
      workflow.dag.delegate.delegate.parents(v).flatMap { getDependencies(workflow, _, true) }
  }
  
  /**
   * explainCallback is used to provide information about why certain realizations
   * are not included in some plan. args are (plan: String)(msg: String)
   */
  def NO_EXPLAIN(planName: Option[String], vertexName: => String, msg: => String, accepted: Boolean) {}
  def getPlannedVertices(workflow: HyperWorkflow,
                         explainCallback: (Option[String], =>String, =>String, Boolean) => Unit = NO_EXPLAIN,
                         errorOnZeroTasks: Boolean = true)
                        (implicit conf: Config): PlanPolicy = {
    
    // Pass 1: One backward pass per task that has a branch graft
    //   tasks that are dependents of branch grafts will always be run for the realization
    //   required by the branch graft
    // Note: Since this algorithm is a bit simplistic, we might actually introduce a few *extra*
    //   realizations that shouldn't be selected.
    debug("Finding graft relaxations...")
    // NOTE: We work directly on the backing HyperDAG, not the PhantomMetaHyperDAG
    val graftRelaxations = new mutable.HashMap[PackedWorkVert, mutable.HashSet[Branch]]
    
    workflow.dag.delegate.delegate.packedWalker.foreach { v: PackedWorkVert =>
      workflow.dag.delegate.delegate.inEdges(v).foreach { hyperedge: WorkflowEdge =>
        debug("Considering hyperedge with sources: " + workflow.dag.sources(hyperedge))
        
        // TODO: Record the realizations for which this graft is required?
        hyperedge.e.foreach { specGroup: SpecGroup =>
          if (specGroup != null && !specGroup.grafts.isEmpty) {
            // recursively find all dependencies of this vertex & add graft relaxations
            val deps: Seq[PackedWorkVert] = getDependencies(workflow, v, true)
            deps.foreach { dep: PackedWorkVert =>
              trace("Grafts are: " + specGroup.grafts)
              graftRelaxations.getOrElseUpdate(dep, new mutable.HashSet[Branch]) ++= specGroup.grafts
            }
          }
        }
      }
    }
    
    debug("Found graft relaxations: " + graftRelaxations)
    
    workflow.plans match {
      case Nil => {
        System.err.println("Using default one-off realization plan: " +
        		"Each realization will have no more than 1 non-baseline branch")
        		

        def explainCallbackCurried(vertexName: => String, msg: => String, accepted: Boolean)
          = explainCallback(Some("default one-off"), vertexName, msg, accepted)

        val planPolicy = OneOff(graftRelaxations)	
        // walk the one-off plan, for the benefit of the explainCallback,
        // not because we actually store any information from it
        workflow.unpackedWalker(planPolicy, explainCallbackCurried).foreach { v: UnpackedWorkVert => ; }  
        planPolicy
      }
      case _ => {
        System.err.println("Finding hyperpaths contained in plan...")
         
        val vertexFilter = new mutable.HashSet[(String,Realization)]
        for (plan: RealizationPlan <- workflow.plans) {
          val planName = plan.name.getOrElse("*anonymous*")
          System.err.println("Finding vertices for plan: %s".format(planName))
          
          // Pass 2: Forward pass through the HyperDAG using a PatternFilter
          //   so that we can discover which realizations are valid at each goal task
          // Note: This isn't as simple as taking the cross-product of branches that have been seen at all dependents
          //   since some branch points may become visible or invisible based on which branches are active.
          val candidates: Map[(String,Realization), RealTask]
            = getCandidates(workflow, plan, explainCallback, graftRelaxations)
          val fronteir = new mutable.Queue[RealTask]
          
          System.err.println("Have %d candidate tasks matching plan's realizations: %s".format(
              candidates.size, candidates.map(_._1).map(_._1).toSet.toSeq.sorted.mkString(" ")))
          
          // Initialize for Pass 3: Take the union over active plans
          //   to obtain the realizations desired for each goal task 
          // initialize with all valid realizations of the goal vertex
          // (realizations have already been filtered during HyperDAG traversal)
          for (goalTask <- plan.goalTasks) {
            val goalRealTasks: Iterable[RealTask] = candidates.filter {
              case ( (tName, _), _) => tName == goalTask
            } map { _._2 }
            System.err.println("Found %d realizations of goal task %s: %s".
              format(goalRealTasks.size, goalTask, goalRealTasks.map(_.realization).mkString(" ")))
              
            // TODO: Now we need to trim off any extra realizations that were introduced for the sake
            // of grafts?
            fronteir ++= goalRealTasks
          }
          
          // Pass 3: Do backward dependency resolution starting at goals
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
          System.err.println("Found %d vertices implied by realization plan %s".format(seen.size, planName))
          
          // this is almost certainly not what the user intended
          if (seen.isEmpty && errorOnZeroTasks) {
            throw new FileFormatException("Plan includes zero tasks", plan.planDef)
          }
          vertexFilter ++= seen.map { task => (task.name, task.realization) }
        }
        System.err.println("Union of all planned vertices has size %d".format(vertexFilter.size))
        VertexFilter(vertexFilter)
      }
    }
  }
}
