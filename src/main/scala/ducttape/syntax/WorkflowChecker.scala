package ducttape.syntax

import ducttape.workflow.HyperWorkflow
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition

import collection._

class WorkflowChecker {
  
  def check(workflow: WorkflowDefinition, confSpecs: Seq[ConfigAssignment]): (Seq[String],Seq[String]) = {
    
    // TODO: Make into exceptions instead?
    val warnings = new mutable.ArrayBuffer[String]
    val errors = new mutable.ArrayBuffer[String]
    
    for (task <- workflow.tasks) {
      for (out <- task.outputs) {
        out.rval match {
          case _: BranchPointDef => {
            errors += "Outputs may not define branch points at %s:%s".
              format(out.rval.declaringFile, out.rval.pos.line)
          }
          case _ => ;
        }
      }
    }
    
    val globals = new mutable.HashSet[String]
    for (a <- confSpecs) {
      if (globals.contains(a.spec.name)) {
        
      }
      globals += a.spec.name
    }
    
    (warnings, errors)
  }
}