package ducttape.workflow

import collection._

import ducttape.hyperdag._
import ducttape.io._
import ducttape.Types._

// generally just a string, but will also
// eventually represent a list of files (for globbing)
class FileInfo(val path: String) {
  override def toString = path
}

class Task(taskDef: TaskDef,
           val inputVals: Map[String,FileInfo], // actual file paths
           val paramVals: Map[String,String]) { // actual param values
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

  private def resolveVar(taskDef: TaskDef,
                         map: Map[String,TaskDef],
                         spec: Spec): (String, Option[TaskDef]) = {

    var rval: RValue = spec.value
    var src: Option[TaskDef] = None
    while(true) {
      rval match {
        case Literal(litValue) => {
          return (litValue, src)
        }
        case Variable(srcTaskName, srcValue) => {
          map.get(srcTaskName) match {
            case Some(srcDef: TaskDef) => {
              src = Some(srcDef)
            }
            case None => {
              // TODO: Line #
              throw new FileFormatException(
                "Source task %s for input %s at task %s not found".format(
                  srcTaskName, spec.name, taskDef.name))
            }
          }
        }
        //case Unbound => {
        //  throw new RuntimeException("Unsupported so far")
        //}
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

      if(vertices.contains(taskDef.name)) {
          // TODO: Line #'s
        throw new FileFormatException("Duplicate task name: %s".format(taskDef.name))
      }

      val paramVals = new mutable.HashMap[String,String]
      for(paramSpec: Spec <-taskDef.params) {
        val (value, _) = resolveVar(taskDef, defMap, paramSpec)
        paramVals += paramSpec.name -> value
      }

      val inputVals = new mutable.HashMap[String,FileInfo]
      val myParents = new mutable.HashSet[TaskDef]
      for(inSpec: Spec <- taskDef.inputs) {
        val (value, srcOpt) = resolveVar(taskDef, defMap, inSpec)
        inputVals += inSpec.name -> new FileInfo(value)
        srcOpt match {
          case Some(src: TaskDef) => {
            myParents += src
          }
          case _ => ;
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
