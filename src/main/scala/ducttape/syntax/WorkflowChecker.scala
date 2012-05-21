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
    
    // check all task-like things declared with the task keyword
    for (task <- workflow.tasks ++ workflow.packages) {
      
      val vars = new mutable.HashMap[String, Spec]
      for (spec: Spec <- task.allSpecs) {
        vars.get(spec.name) match {
          case Some(prev) => {
            errors += "Task variable %s originally defined at %s:%d redefined at %s:%d".
                  format(prev.name, prev.declaringFile, prev.pos.line, spec.declaringFile, spec.pos.line)
          }
          case None => ;
        }
        vars += spec.name -> spec
      }
      
      // don't allow branch points on outputs
      for (out <- task.outputs) {
        out.rval match {
          case _: BranchPointDef => {
            errors += "Outputs may not define branch points at %s:%d".
              format(out.rval.declaringFile, out.rval.pos.line)
          }
          case _ => ;
        }
      }
    }
    
    val globals = new mutable.HashMap[String, Spec]
    for (a <- confSpecs) {
      globals.get(a.spec.name) match {
        case Some(prev) => {
          errors += "Global variable originally defined at %s:%d redefined at %s:%d".
                format(prev.declaringFile, prev.pos.line, a.spec.declaringFile, a.spec.pos.line)
        }
        case None => ;
      }
      globals += a.spec.name -> a.spec
    }
    
    // don't allow globals to have a name
    for (globalBlock <- workflow.globalBlocks) globalBlock.name match {
      case None => ; // good
      case Some(name) => {
        errors += "Global variable block defined at %s:%d has a name. This is not allowed.".
                format(globalBlock.declaringFile, globalBlock.pos.line, globalBlock.declaringFile)
      }
    }
    
    (warnings, errors)
  }
}