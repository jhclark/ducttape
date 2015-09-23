// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.cli

import collection._
import ducttape.syntax.Namespace
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
import ducttape.workflow.RealTaskId
import ducttape.workflow.SpecGroup
import ducttape.workflow.Types.UnpackedWorkVert
import ducttape.workflow.Types.PackedWorkVert
import ducttape.workflow.Types.WorkflowMetaEdge
import ducttape.workflow.Types.WorkflowEdge
import ducttape.workflow.RealizationPlan
import ducttape.workflow.TaskTemplate
import ducttape.workflow.builder.WorkflowBuilder
import ducttape.versioner.WorkflowVersionInfo
import ducttape.util.Optional

import grizzled.slf4j.Logging

// TODO: Disconnect from CLI and move to workflow package
object Plans extends Logging {
  
  // implemented the second forward pass through the HyperDAG
  // using a PatternFilter
  private def getCandidates(workflow: HyperWorkflow,
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
        val task: RealTask = taskT.toRealTask(v)
        trace(s"Found new candidate: ${task}")
        candidates += (task.name, task.realization) -> task
    })
    candidates
  }
    
  /**
   * explainCallback is used to provide information about why certain realizations
   * are not included in some plan. args are (plan: String)(msg: String)
   *
   * planNames specifies specific plan names that should be used. if not specified, the union of all plans is used.
   */
  private def NO_EXPLAIN(planName: Option[String], vertexName: => String, msg: => String, accepted: Boolean) {}
  def getPlannedVertices(workflow: HyperWorkflow,
                         explainCallback: (Option[String], =>String, =>String, Boolean) => Unit = NO_EXPLAIN,
                         errorOnZeroTasks: Boolean = true,
                         planNames: Option[Seq[String]] = None,
                         verbose: Boolean = false)
                        : PlanPolicy = {
    
    debug("Finding graft relaxations...")

    // TODO: Do this in two phases: 1) get direct grafts 2) propagate to parents
    val immediateGrafts = new mutable.HashMap[PackedWorkVert, mutable.HashSet[Branch]]

    // 1) Accumulate graft relaxations:
    //    get grafts at each vertex (we'll propagate them to dependencies next).
    //    we must do this because we don't want to force the user to mention grafts 
    //    explicitly in the plan
    // TODO: Packed walker seems to have odd behavior
    //
    // Pass 1: One backward pass per task that has a branch graft
    //   tasks that are dependents of branch grafts will always be run for the realization
    //   required by the branch graft
    // Note: Since this algorithm is a bit simplistic, we might actually introduce a few *extra*
    //   realizations that shouldn't be selected. We will weed these out in the second pass.
    for (v: PackedWorkVert <- workflow.dag.delegate.delegate.vertices) {
      workflow.dag.delegate.delegate.inEdges(v).foreach { hyperedge: WorkflowEdge =>
        trace(s"Find graft relaxations for ${v}: Considering hyperedge with sources: ${workflow.dag.sources(hyperedge)}")

        // TODO: Record the realizations for which this graft is required -- if we want to save some time
        //       (It could occur inside a nested branch point)
        hyperedge.e.foreach { specGroup: SpecGroup =>
          trace(s"Find graft relaxations for ${v}: Considering edge: ${specGroup}")
          if (specGroup != null && !specGroup.grafts.isEmpty) {
            immediateGrafts.getOrElseUpdate(v, new mutable.HashSet) ++= specGroup.grafts
          }
        }
      }
    }

    debug("Propagating graft dependencies to recursive dependencies...")

    // NOTE: We work directly on the backing HyperDAG, not the PhantomMetaHyperDAG
    val graftRelaxations = new mutable.HashMap[PackedWorkVert, mutable.HashSet[Branch]]

    // TODO: This is overzealous since these are not necessarily
    //   all dependencies in the unpacked DAG
    // "v" is the initial vertex that requires this graft relaxation
    // "dep" is the current dependency we've reached
    def visitDependencies(v: PackedWorkVert, dep: PackedWorkVert, grafts: Set[Branch]) {
      val relaxationsAtDep = graftRelaxations.getOrElseUpdate(dep, new mutable.HashSet)
      // note: this check is key to making this code block efficient
      if (!grafts.forall { graft: Branch => relaxationsAtDep.contains(graft) }) {
        debug(s"Propagate graft relaxations of ${v}: Dependency ${dep} added new grafts: ${grafts}")
        grafts.foreach { graft: Branch => relaxationsAtDep += graft }
        workflow.dag.delegate.delegate.parents(dep).foreach(visitDependencies(v, _, grafts))
      } else {
        trace(s"Propagate graft relaxations of ${v}: Dependency ${dep} already has grafts: ${grafts}")
      }
    }

    // TODO: Sort vertices?
    // Iterate over tasks that have any grafts
    for ( (v: PackedWorkVert, grafts: Set[Branch]) <- immediateGrafts) {
      // recursively find all dependencies of this vertex & add graft relaxations
      visitDependencies(v, v, grafts)
    }
    
    debug {
      for ( (v, set) <- graftRelaxations) {
        debug(s"Found graft relaxation: ${v} -> ${set.toString}")
      }
      s"Found ${graftRelaxations.size} graft relaxations total."
    }
    
    workflow.plans match {
      case Nil => {        		
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
         
        val vertexFilter = new mutable.HashSet[(Namespace,Realization)]
        val plans: Seq[RealizationPlan] = planNames match {
          case None => workflow.plans // use union of all plans if none are specified
          case Some(names) => {
            val requestedNames: Set[String] = names.toSet
            workflow.plans.filter { plan: RealizationPlan =>
              plan.name match {
                case None => false
                case Some(name) => requestedNames.contains(name)
              }
            } match {
              // TODO: Change to CLI exception?
              case Seq() => {
                val candidates = workflow.plans.map(_.name.getOrElse("*anonymous*"))
                throw new RuntimeException(s"One of the specified plans was not found: '${planNames.mkString(" ")}'. Candidates are: ${candidates.mkString(" ")}")
              }
              case matches @ _ => matches
            }
          }
        }
        for (plan: RealizationPlan <- plans) {
          // Pass 2: Forward pass through the HyperDAG using a PatternFilter
          //   so that we can discover which realizations are valid at each goal task
          // Note: This isn't as simple as taking the cross-product of branches that have been seen at all dependents
          //   since some branch points may become visible or invisible based on which branches are active.
          val candidates: Map[(String,Realization), RealTask]
            = getCandidates(workflow, plan, explainCallback, graftRelaxations)
          
          if (verbose) {
            val matches = candidates.map(_._1).map(_._1).toSet.toSeq.sorted
            System.err.println(s"Have ${candidates.size} candidate tasks matching plan's realizations: ${matches.mkString(" ")}")
          }
          
          // Initialize for Pass 3: Take the union over active plans, working inside the *unpacked* workflow
          //   to obtain the realizations desired for each goal task 
          // initialize with all valid realizations of the goal vertex
          // (realizations have already been filtered during HyperDAG traversal)
          val fronteir = new mutable.Queue[RealTask]
          for (goalTask <- plan.goalTasks) {
            val candidateGoalRealTasks: Iterable[RealTask] = candidates.filter {
              case ( (tName, _), _) => tName == goalTask
            } map { _._2 }

            // Because of graft relaxations, we've actually been a bit too loose in our constraints here,
            // we also need to filter these candidate realizations based on the cross-product
            // of possible realizations allowed under the plan
            // i.e. each branch of a goal task must either be explicitly mentioned in a plan
            // or it must be a baseline branch of a branch point that is not mentioned in the plan
            var strictFilter = PatternFilter(plan.realizations, graftRelaxations = Map.empty)
            val goalRealTasks: Iterable[RealTask] = candidateGoalRealTasks.filter { realTask: RealTask =>
              var v = workflow.toPackedVertex(realTask.taskT)
              strictFilter.matches(v, realTask.realization.branches.toSet)
            }

            if (verbose) {
              val goalRealizations: Iterable[Realization] = goalRealTasks.map(_.realization)
              System.err.println(s"Found ${goalRealTasks.size} realizations of goal task ${goalTask}: ${goalRealizations.mkString(" ")}")
            }
              
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
                  debug(s"Antecedents of ${task} are ${antTasks.mkString(" ")}")
                fronteir ++= antTasks
              } catch {
                case e: NoSuchElementException => {
                  throw new RuntimeException(s"Error while trying to find antecedent tasks of ${task}", e)
                }
              }
            }
            // mark this task as seen
            seen += task
          }
          
          // everything we saw is required to execute this realization plan to its goal vertices
          System.err.println(s"Found ${seen.size} vertices implied by realization plan ${plan.fullName}")
          
          // this is almost certainly not what the user intended
          if (seen.isEmpty && errorOnZeroTasks) {
            throw new FileFormatException("Plan includes zero tasks", plan.planDef)
          }
          vertexFilter ++= seen.map { task => (task.namespace, task.realization) }
        }
        System.err.println(s"Union of all planned vertices has size ${vertexFilter.size}")
        val verticesToInclude: Set[RealTaskId] = vertexFilter.map { case (namespace, real) =>
          new RealTaskId(namespace, real.toCanonicalString())
        }
        VertexFilter(verticesToInclude)
      }
    }
  }
}
