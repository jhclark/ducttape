package ducttape.exec

import java.io.File
import ducttape.util.Environment
import ducttape.workflow.Realization
import ducttape.util.Shell
import ducttape.syntax.BashCode
import ducttape.syntax.FileFormatException
import ducttape.syntax.AbstractSyntaxTree.ShellCommands
import ducttape.syntax.AbstractSyntaxTree.ActionDef
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.SubmitterDef
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.workflow.Branch
import ducttape.util.BashException
import ducttape.util.Files
import scala.collection.LinearSeq
import ducttape.workflow.SpecTypes._

class Submitter(submitters: Seq[SubmitterDef]) {
  
  // TODO: Really, this should be resolved during workflow building and
  // we should never pass the workflow definition anywhere else...
  private def getSubmitter(submitterSpec: ResolvedLiteralSpec): SubmitterDef = {
    
    val submitterName = submitterSpec.srcSpec.rval.value
    submitters.find{ s => s.name == submitterName } match {
      case Some(s) => s
      case None => throw new FileFormatException("Submitter %s not defined", List(submitterSpec.mySpec, submitterSpec.srcSpec))
    }
  }
  
  private def getDefaultSubmitter(submitterName: String, requiredBy: TaskDef): SubmitterDef = {
    submitters.find{ s => s.name == submitterName } match {
      case Some(s) => s
      case None => throw new FileFormatException("Default submitter %s not defined (required by task %s)".format(submitterName, requiredBy.name), requiredBy)
    }
  }
  
  private def getRunAction(submitterDef: SubmitterDef): ActionDef = {
    submitterDef.actions.find{ action => action.name == "run"} match {
      case Some(action: ActionDef) => action
      case None => throw new FileFormatException("No 'run' action defined for submitter %s".format(submitterDef.name), submitterDef)
    }
  }

  def run(taskEnv: FullTaskEnvironment) {

    // TODO: Check in ducttape/defaults for default submitters/versioners

    val dotParams: Seq[ResolvedLiteralSpec] = taskEnv.task.paramVals.filter(_.mySpec.dotVariable)
    val dotParamsEnv: Seq[(String,String)] = dotParams.map{ p => (p.mySpec.name, p.srcSpec.rval.value) }
    
    val runAction = {
      val submitterDef = dotParams.find{p: ResolvedLiteralSpec => p.mySpec.name == "submitter"} match {
        case Some(p) => getSubmitter(p)
        case None => getDefaultSubmitter("shell", taskEnv.task.taskDef)
      }
      getRunAction(submitterDef)
    }

    // TODO: Run also requires the real params from the task
    // TODO: Make sure no params have name conflicts
    
    // TODO: Grab all dot params from this task
    val env: Seq[(String,String)] = Seq(
        ("TASK", taskEnv.task.name),
        ("REALIZATION", taskEnv.task.realization.toString),
        ("CONFIGURATION", taskEnv.dirs.confName.getOrElse(""))) ++ dotParamsEnv ++ taskEnv.env
        
    // To prevent some strange quoting bugs, treat COMMANDS specially and directly substitute it
    // TODO: Any other mangling that might be necessary here?
    val code = runAction.commands.toString.replace("$COMMANDS", taskEnv.task.commands.toString).replace("${COMMANDS}", taskEnv.task.commands.toString)
    
    val exitCode = Shell.run(code, taskEnv.where, env, taskEnv.stdoutFile, taskEnv.stderrFile)
    Files.write("%d".format(exitCode), taskEnv.exitCodeFile)
    if (exitCode != 0) {
      throw new BashException("Task %s/%s failed".format(taskEnv.task.name, taskEnv.task.realization))
    }
  }
}
