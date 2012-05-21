package ducttape.syntax

import ducttape.exec.Submitter
import ducttape.exec.Versioners
import ducttape.exec.PackageVersionerInfo
import ducttape.workflow.HyperWorkflow
import ducttape.syntax.AbstractSyntaxTree.BranchPointDef
import ducttape.syntax.AbstractSyntaxTree.ConfigAssignment
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.ConfigDefinition
import ducttape.syntax.AbstractSyntaxTree.PackageDef
import ducttape.syntax.AbstractSyntaxTree.VersionerDef
import ducttape.syntax.AbstractSyntaxTree.ActionDef
import ducttape.syntax.AbstractSyntaxTree.Literal

import collection._

class WorkflowChecker {
  
  def check(workflow: WorkflowDefinition, confSpecs: Seq[ConfigAssignment]): (Seq[String],Seq[String]) = {
    
    // TODO: Make into exceptions instead?
    val warnings = new mutable.ArrayBuffer[String]
    val errors = new mutable.ArrayBuffer[String]
    
    // check all task-like things declared with the task keyword
    for (task: TaskDef <- workflow.tasks ++ workflow.packages) {
      
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
      for (out: Spec <- task.outputs) {
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
    for (a: ConfigAssignment <- confSpecs) {
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
    for (globalBlock: ConfigDefinition <- workflow.globalBlocks) globalBlock.name match {
      case None => ; // good
      case Some(name) => {
        errors += "Global variable block defined at %s:%d has a name. This is not allowed.".
                format(globalBlock.declaringFile, globalBlock.pos.line, globalBlock.declaringFile)
      }
    }
    
    // check versioners to make sure they're sane
    // TODO: Make a static check that all these dot variables are literal (not branch points, etc.)
    for (v: VersionerDef <- workflow.versioners) {
      {
        if (!v.packages.isEmpty)
          errors += "Versioners cannot define packages: Versioner '%s'".format(v.name)
        if (!v.inputs.isEmpty)
          errors += "Versioners cannot define inputs: Versioner '%s'".format(v.name)
        if (!v.outputs.isEmpty)
          errors += "Versioners cannot define outputs: Versioner '%s'".format(v.name)
      }
      
      val info = new PackageVersionerInfo(v)
      
      //TODO: File:line info
      {
        val checkout: ActionDef = info.checkoutDef
        if (!checkout.packages.isEmpty)
          errors += "The checkout action cannot define packages: Versioner '%s'".format(v.name)
        if (!checkout.inputs.isEmpty)
          errors += "The checkout action cannot define inputs: Versioner '%s'".format(v.name)
        if (checkout.outputs.map(_.name) != Seq("dir"))
          errors += "The checkout action must define exactly one output called 'dir': Versioner '%s'".format(v.name)
        if (!checkout.params.isEmpty)
          errors += "The checkout action cannot define parameters: Versioner '%s'".format(v.name)
      }
      
      {
        val repoVer: ActionDef = info.repoVersionDef
        if (!repoVer.packages.isEmpty)
          errors += "The repo_version action cannot define packages: Versioner '%s'".format(v.name)
        if (!repoVer.inputs.isEmpty)
          errors += "The repo_version action cannot define inputs: Versioner '%s'".format(v.name)
        if (repoVer.outputs.map(_.name) != Seq("version"))
          errors += "The repo_version action must define exactly one output called 'version': Versioner '%s'".format(v.name)
        if (!repoVer.params.isEmpty)
          errors += "The repo_version action cannot define parameters: Versioner '%s'".format(v.name)
      }
      
      {
        val localVer: ActionDef = info.localVersionDef
        if (!localVer.packages.isEmpty)
          errors += "The local_version action cannot define packages: Versioner '%s'".format(v.name)
        if (!localVer.inputs.isEmpty)
          errors += "The local_version action cannot define inputs: Versioner '%s'".format(v.name)
        if (localVer.outputs.map(_.name).toSet != Set("version", "date"))
          errors += "The local_version action must define exactly two outputs called 'version' and 'date': Versioner '%s'".format(v.name)
        if (!localVer.params.isEmpty)
          errors += "The local_version action cannot define parameters: Versioner '%s'".format(v.name)
      }
    }
    
    // make sure that each package has defined all of the dot variables required by its versioner
    val versionerDefs = workflow.versioners.map { v => (v.name, v) }.toMap
    for (packageDef: PackageDef <- workflow.packages) {
      
      for (param <- packageDef.params) param.rval match {
        case _: Literal => ;
        case _ => errors += "Package parameters must be literals: Package '%s'".format(packageDef.name)
      }
      
      try {
        // constructor throws if versioner cannot be found or correct actions are not defined
        val versionerDef = Versioners.getVersioner(packageDef, versionerDefs)
        val info = new PackageVersionerInfo(versionerDef)
        
        // check that package defines as dot parameters all parameters required by versioner
        val dotParams: Set[String] = packageDef.params.filter(_.dotVariable).map(_.name).toSet
        for (requiredParam: Spec <- versionerDef.params) {
          if (!dotParams.contains(requiredParam.name)) {
            // TODO: file:line info
            errors += "Package '%s' does not define dot parameter '%s' required by versioner '%s'".format(
              packageDef.name, requiredParam.name, versionerDef.name)
          }
        }
        
      } catch {
        case e: FileFormatException => {
          // TODO: File:line info
          errors += e.getMessage
        }
      }
    }
    
    // TODO: Check for each task having the params defined for each of its versioners
    
    (warnings, errors)
  }
}