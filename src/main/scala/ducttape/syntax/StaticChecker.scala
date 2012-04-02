package ducttape.syntax

import collection._
import ducttape.Config
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition

object ErrorBehavior extends Enumeration {
  type ErrorBehavior = Value
  val Ignore, Warn, Error = Value
}
import ErrorBehavior._

class StaticChecker(conf: Config,
                    undeclaredBehavior: ErrorBehavior,
                    unusedBehavior: ErrorBehavior) {
  
  /**
   * Returns a tuple of (warnings, errors)
   */
  def check(wd: WorkflowDefinition): (Seq[String], Seq[String]) = {
    val warnings = new mutable.ArrayBuffer[String]
    val errors = new mutable.ArrayBuffer[String]
    for(task <- wd.tasks) {
      val (w, e) = check(task)
      warnings ++= w
      errors ++= e
    }
    (warnings, errors)
  }
  
  /**
   * Returns a tuple of (warnings, errors)
   */
  def check(taskDef: TaskDef): (Seq[String], Seq[String]) = {
    val commands: BashCode = taskDef.commands
    val usedVars: Set[String] = commands.vars
    
    val definedVars: Set[String] = taskDef.header.specsList.flatMap(_.specs.filterNot(_.dotVariable).map(_.name)).toSet
    
    val warnings = new mutable.ArrayBuffer[String]
    val errors = new mutable.ArrayBuffer[String]
    
    // check for use of undeclared bash variables
    for(usedVar <- usedVars) {
      if(!definedVars.contains(usedVar)) {
        // TODO: Get position information from bash
        val msg = "Potential use of undefined variable: %s at task %s".format(usedVar, taskDef.name)
        undeclaredBehavior match {
          case Ignore => ;
          case Warn => warnings += msg
          case Error => errors += msg
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
          case Warn => warnings += msg
          case Error => errors += msg
        }
      }
    }
    
    (warnings, errors)
  }
}