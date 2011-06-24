package ducttape.workflow

import collection._

import ducttape.hyperdag._
import ducttape.io._
import ducttape.Types._

class Task(val taskDef: TaskDef,
           val inputVals: Seq[(Spec,Spec,TaskDef)], // (mySpec,srcSpec,srcTaskDef)
           val paramVals: Seq[(Spec,LiteralSpec,TaskDef)]) { // (mySpec,srcSpec,srcTaskDef)
  def name = taskDef.name
  def comments = taskDef.comments
  def inputs = taskDef.inputs
  def outputs = taskDef.outputs
  def params = taskDef.params
  def commands = taskDef.commands
  
  override def toString = name
}
class Branch(val name: String) {
  override def toString = "Branch: " + name
}

object WorkflowBuilder {

  // the resolved Spec is guaranteed to be a literal
  private def resolveVar(taskDef: TaskDef,
                         map: Map[String,TaskDef],
                         spec: Spec)
  : (LiteralSpec, TaskDef) = {

    var curSpec: Spec = spec
    var src: TaskDef = taskDef
    while(true) {
      curSpec.rval match {
        case Literal(litValue) => {
          val litSpec = curSpec.asInstanceOf[LiteralSpec] // guaranteed to succeed
          return (litSpec, src)
        }
        case Variable(srcTaskName, srcValue) => {
          map.get(srcTaskName) match {
            case Some(srcDef: TaskDef) => {
              src = srcDef
              // TODO: Lookup source variable name
            }
            case None => {
              // TODO: Line #
              throw new FileFormatException(
                "Source task %s for input %s at task %s not found".format(
                  srcTaskName, spec.name, taskDef.name))
            }
          }
        }
        case Unbound() => {
          throw new RuntimeException("Unsupported so far")
        }
      }
    }
    throw new Error("Unreachable")
  }

  // create dependency pointers based on workflow definition  
  def build(wd: WorkflowDefinition): HyperWorkflow = {

    val NO_BRANCH = new Branch("")

    val defMap = new mutable.HashMap[String,TaskDef]
    for(t <- wd.tasks) {
      defMap += t.name -> t
    }

    // (task, parents)
    val parents = new mutable.HashMap[Task, Seq[TaskDef]]
    val vertices = new mutable.HashMap[String,PackedVertex[Task]]
    val dag = new PackedDagBuilder[Task,Branch,Null]

    // first, create tasks, but don't link them in graph yet
    for(taskDef <- wd.tasks) {

      // TODO: Check for all inputs/outputs/param names being unique in this step

      if(vertices.contains(taskDef.name)) {
          // TODO: Line #'s
        throw new FileFormatException("Duplicate task name: %s".format(taskDef.name))
      }

      val paramVals = new mutable.ListBuffer[(Spec,LiteralSpec,TaskDef)]
      for(paramSpec: Spec <-taskDef.params) {
        val (srcSpec, srcTaskDef) = resolveVar(taskDef, defMap, paramSpec)
        paramVals.append( (paramSpec, srcSpec, srcTaskDef) )
      }

      val inputVals = new mutable.ListBuffer[(Spec,Spec,TaskDef)]
      val myParents = new mutable.HashSet[TaskDef]
      for(inSpec: Spec <- taskDef.inputs) {
        val (srcSpec, srcTaskDef) = resolveVar(taskDef, defMap, inSpec)
        inputVals.append( (inSpec, srcSpec, srcTaskDef) )
        if(srcTaskDef != taskDef) {
            myParents += srcTaskDef
        }
      }

      val task = new Task(taskDef, inputVals, paramVals)
      parents += task -> myParents.toSeq
      vertices += task.name -> dag.add(task)
    }

    println("Got %d vertices".format(vertices.size))

    // now link the tasks in the graph
    for(v <- vertices.values) {
      val task = v.value
      val edges = new mutable.ListBuffer[(PackedVertex[Task],Null)]
      for(parentTaskDef: TaskDef <- parents(task)) {
        val parentVert = vertices(parentTaskDef.name)
        edges.append( (parentVert, null) )
      }

      if(!edges.isEmpty) {
        dag.add(NO_BRANCH, edges.toList, v)
      }
    }

    // TODO: For params, we can resolve these values *ahead*
    // of time, prior to scheduling (but keep relationship info around)
    new HyperWorkflow(dag.build)
  }
}
