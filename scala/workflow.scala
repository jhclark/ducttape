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

  case class ResolveMode;
  case class InputMode extends ResolveMode;
  case class ParamMode extends ResolveMode;
  case class OutputMode extends ResolveMode;

  // the resolved Spec is guaranteed to be a literal for params
  private def resolveParam(taskDef: TaskDef,
                             map: Map[String,TaskDef],
                             spec: Spec)
  : (LiteralSpec, TaskDef) = {
    resolveVar(taskDef, map, spec, ParamMode()).asInstanceOf[(LiteralSpec,TaskDef)]
  }

  private def resolveVar(taskDef: TaskDef,
                         map: Map[String,TaskDef],
                         spec: Spec,
                         mode: ResolveMode)
  : (Spec, TaskDef) = {

    var curSpec: Spec = spec
    var src: TaskDef = taskDef
    while(true) {
      curSpec.rval match {
        case Literal(litValue) => {
          val litSpec = curSpec.asInstanceOf[LiteralSpec] // guaranteed to succeed
          return (litSpec, src)
        }
        case Variable(srcTaskName, srcOutName) => {
          map.get(srcTaskName) match {
            case Some(srcDef: TaskDef) => {
              // determine where to search when resolving this variable's parent spec
              val specSet = mode match {
                case InputMode() => srcDef.outputs
                case ParamMode() => srcDef.params
              }
              // now search for the parent spec
              specSet.find(outSpec => outSpec.name == srcOutName) match {
                case Some(srcSpec) => curSpec = srcSpec
                case None => {
                  // TODO: Line # of srcDef & spec
                  throw new FileFormatException(
                    "Output %s at source task %s for input %s at task %s not found".format(
                      srcOutName, srcTaskName, spec.name, taskDef.name))
                }
              }
              // assign after we've gotten a chance to print error messages
              src = srcDef
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
          mode match {
            case InputMode() => return (curSpec, src)
            case _ => throw new RuntimeException("Unsupported unbound variable: %s".format(curSpec.name))
          }
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
        val (srcSpec, srcTaskDef) = resolveParam(taskDef, defMap, paramSpec)
        paramVals.append( (paramSpec, srcSpec, srcTaskDef) )
      }

      val inputVals = new mutable.ListBuffer[(Spec,Spec,TaskDef)]
      val myParents = new mutable.HashSet[TaskDef]
      for(inSpec: Spec <- taskDef.inputs) {
        val (srcSpec, srcTaskDef) = resolveVar(taskDef, defMap, inSpec, InputMode())
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
