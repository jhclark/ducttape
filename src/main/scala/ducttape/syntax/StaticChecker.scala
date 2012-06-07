package ducttape.syntax

import collection._
import ducttape.syntax.AbstractSyntaxTree._

object ErrorBehavior extends Enumeration {
  type ErrorBehavior = Value
  val Ignore, Warn, Error = Value
}
import ErrorBehavior._

class StaticChecker(undeclaredBehavior: ErrorBehavior,
                    unusedBehavior: ErrorBehavior) {
  
  /**
   * Returns a tuple of (warnings, errors)
   */
  def check(wd: WorkflowDefinition): (Seq[FileFormatException], Seq[FileFormatException]) = {
    
    val warnings = new mutable.ArrayBuffer[FileFormatException]
    val errors = new mutable.ArrayBuffer[FileFormatException]
    
    // check for unsupported block types that are in the grammar, but
    // not yet implemented
    for (block <- wd.blocks) block match {
      case funcCall: CallDefinition => errors += new FileFormatException("Function calls are not supported yet", funcCall)
  
      case groupLike: GroupDefinition => {
        groupLike.keyword match {
          case "group" => errors += new FileFormatException("Group blocks are not supported yet", groupLike)
          case "summary" => errors += new FileFormatException("Summary blocks are not supported yet", groupLike)
          case "branchpoint" => errors += new FileFormatException("Branchpoint blocks are not supported yet", groupLike)
          case "submitter" => ;
          case "versioner" => ;
        }
      }

      case taskLike: TaskDef => {
        taskLike.keyword match {
          case "baseline" => errors += new FileFormatException("Baseline blocks are not supported yet", taskLike)
          case "branch" => errors += new FileFormatException("Branch blocks are not supported yet", taskLike)
          case "action" => errors += new FileFormatException("Action blocks are not supported yet", taskLike)
          case "of" => errors += new FileFormatException("'of' blocks are not supported yet", taskLike)
          case "func" => errors += new FileFormatException("Function definitions are not supported yet", taskLike)
          case "package" => ;
          case "task" => ;
          case _ => throw new RuntimeException("Unrecognized task-like block type: " + taskLike.keyword)
        }
      }
    }
    
    for (task: TaskDef <- wd.tasks) {
      val (w, e) = check(task)
      warnings ++= w
      errors ++= e
    }
    
    (warnings, errors)
  }
  
  /**
   * Returns a tuple of (warnings, errors)
   */
  def check(taskDef: TaskDef): (Seq[FileFormatException], Seq[FileFormatException]) = {
    val warnings = new mutable.ArrayBuffer[FileFormatException]
    val errors = new mutable.ArrayBuffer[FileFormatException]
        
    // first check for things not allowed in ducttape
    for (spec <- taskDef.inputs) spec.rval match {
      case Literal(path) => {
        if (path == "") {
          errors += new FileFormatException("Empty input name not allowed", spec)
        }
      }
      case _ => ;
    }
    for (spec <- taskDef.outputs) spec.rval match {
      case Literal(path) => {
        if (path == "") {
          errors += new FileFormatException("Empty output name not allowed", spec)
        }
      }
      case _ => ;
    }
    
    // now check for bash issues
    val commands: BashCode = taskDef.commands
    val usedVars: Set[String] = commands.vars
    
    val definedVars: Set[String] = taskDef.header.specsList.flatMap(_.specs.filterNot(_.dotVariable).map(_.name)).toSet
    
    // check for use of undeclared bash variables
    for (usedVar <- usedVars) {
      if (!definedVars.contains(usedVar)) {
        // TODO: Get position information from bash
        val msg = "Potential use of undefined variable: %s at task %s".format(usedVar, taskDef.name)
        undeclaredBehavior match {
          case Ignore => ;
          case Warn => warnings += new FileFormatException(msg, taskDef)
          case Error => errors += new FileFormatException(msg, taskDef)
        }
      }
    }
    
    // check for not using declared variables
    for(definedVar <- definedVars) {
      if(!usedVars.contains(definedVar)) {
        // TODO: Get position information from bash
        val msg = "Unused variable: %s at task %s".format(definedVar, taskDef.name)
        unusedBehavior match {
          case Ignore => ;
          case Warn => warnings += new FileFormatException(msg, taskDef)
          case Error => errors += new FileFormatException(msg, taskDef)
        }
      }
    }
    
    (warnings, errors)
  }
}