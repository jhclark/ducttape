package ducttape.viz

import collection._
import ducttape.syntax.AbstractSyntaxTree.TaskDef

object WorkflowViz {
  import ducttape.versioner._
  import ducttape.workflow._
  import ducttape.workflow.SpecTypes._
  import ducttape.workflow.Types._

  def toPackedGraphViz(workflow: HyperWorkflow) = {
    val str = new StringBuilder(1000)
    str ++= "digraph G {\n"

    def getName(t: Option[TaskDef]) = GraphViz.escape(t match {
      case Some(taskDef) => taskDef.name.name
      case None => "Literal"
    })

    // first, list vertices
    for (v: PackedWorkVert <- workflow.packedWalker.iterator) {
      val taskT: TaskTemplate = v.value.get
      val taskName = getName(Some(taskT.taskDef))
      val color = "dodgerblue1"
      val QUOT = "\""
      str ++= s"${QUOT}${taskName}${QUOT} [fillcolor=${color},style=filled];\n"
    }

    // now, list edges
    for (v: PackedWorkVert <- workflow.packedWalker.iterator) {
      val taskT: TaskTemplate = v.value.get
      val taskName = getName(Some(taskT.taskDef))
      val color = "dodgerblue1"
      val QUOT = "\""
      str ++= s"${QUOT}${taskName}${QUOT} [fillcolor=${color},style=filled];\n"

      val parents: Map[String, Seq[SpecPair]] = taskT.inputVals.groupBy { inputVal => getName(inputVal.srcTask) }
      parents.foreach { case (parentName: String, inputVals: Seq[SpecPair]) =>
        if (parentName != taskName) {
          val DOLLAR = "$"
          def toString(sp: SpecPair) = sp.srcTask match {
            case Some(srcTask) => s"${sp.origSpec.name}=${DOLLAR}${sp.srcSpec.name}@${srcTask.name}"
            case None => s"${sp.origSpec.name}=${DOLLAR}${sp.srcSpec.name} (=${sp.srcSpec.rval})"
          }
          val edgeLabel: String = inputVals.map(toString(_)).mkString("\\n")

          val QUOT = "\""
          str ++= s"${QUOT}${parentName}${QUOT} -> ${QUOT}${taskName}${QUOT} [label=${QUOT}${edgeLabel}${QUOT}];\n"
        }
      }
    }

    str ++= "}\n"
    str.toString
  }


  def toGraphViz(workflow: HyperWorkflow,
                 planPolicy: PlanPolicy,
                 completed: Set[(String,Realization)] = Set.empty,
                 running: Set[(String,Realization)] = Set.empty,
                 failed: Set[(String,Realization)] = Set.empty) = {

    val str = new StringBuilder(1000)
    str ++= "digraph G {\n"

    def getName(t: Option[TaskDef], r: Realization)
      = GraphViz.escape(s"${t.getOrElse("Literal")}/${r.toFullString(hashLongNames=false)}")

    // first, list vertices
    for (v: UnpackedWorkVert <- workflow.unpackedWalker(planPolicy).iterator) {
      val taskT: TaskTemplate = v.packed.value.get
      val task: RealTask = taskT.toRealTask(v)
      val color = (task.name, task.realization) match {
        case t if completed(t) => "dodgerblue1"
        case t if running(t) => "darkolivegreen4"
        case t if failed(t) => "firebrick"
        case _ => "white"
      }
      val taskName = getName(Some(task.taskDef), task.realization)
      val QUOT = "\""
      str ++= s"${QUOT}${taskName}${QUOT} [fillcolor=${color},style=filled];\n"
    }

    // now list edges
    for (v: UnpackedWorkVert <- workflow.unpackedWalker(planPolicy).iterator) {
      val taskT: TaskTemplate = v.packed.value.get

      val task: RealTask = taskT.toRealTask(v)
      val taskName = getName(Some(task.taskDef), task.realization)
      val parents: Map[String, Seq[ResolvedSpec]] = task.inputVals.groupBy { inputVal => getName(inputVal.srcTask, inputVal.srcReal) }
      parents.foreach { case (parentName: String, inputVals: Seq[ResolvedSpec]) =>
        if (parentName != taskName) {
          val DOLLAR = "$"
          def toString(sp: ResolvedSpec) = sp.srcTask match {
            case Some(srcTask) => s"${sp.origSpec.name}=${DOLLAR}${sp.srcSpec.name}@${srcTask.name}"
            case None => s"${sp.origSpec.name}=${DOLLAR}${sp.srcSpec.name} (=${sp.srcSpec.rval})"
          }
          val edgeLabel: String = inputVals.map(toString(_)).mkString("\\n")

          val QUOT = "\""
          str ++= s"${QUOT}${parentName}${QUOT} -> ${QUOT}${taskName}${QUOT} [label=${QUOT}${edgeLabel}${QUOT}];\n"
        }
      }
    }

    str ++= "}\n"
    str.toString
  }
}
