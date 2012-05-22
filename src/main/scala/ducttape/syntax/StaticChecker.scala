package ducttape.syntax

import collection._
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.syntax.AbstractSyntaxTree.Literal
import ducttape.syntax.AbstractSyntaxTree.PlanDefinition
import ducttape.syntax.AbstractSyntaxTree.CrossProduct
import ducttape.syntax.AbstractSyntaxTree.BranchPointRef

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