// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
import grizzled.slf4j.Logging
import ducttape.workflow.RealTask

object Submitter {
  // some special variables are passed without user intervention
  val SPECIAL_VARIABLES = Set("COMMANDS", "TASK_VARIABLES", "TASK", "REALIZATION", "CONFIGURATION")
}

class Submitter(submitters: Seq[SubmitterDef]) extends Logging {
  
  // TODO: Really, this should be resolved during workflow building and
  // we should never pass the workflow definition anywhere else...
  private def getSubmitter(submitterSpec: ResolvedLiteralSpec): SubmitterDef = {
    
    val submitterName = submitterSpec.srcSpec.rval.value
    submitters.find { s => s.name.toString == submitterName } match {
      case Some(s) => s
      case None => throw new FileFormatException(s"Submitter ${submitterName} not defined",
                     List(submitterSpec.origSpec, submitterSpec.srcSpec))
    }
  }
  
  private def getDefaultSubmitter(submitterName: String, requiredBy: TaskDef): SubmitterDef = {
    submitters.find { s: SubmitterDef => s.name.toString == submitterName } match {
      case Some(s) => s
      case None => throw new FileFormatException(
        s"Default submitter '${submitterName}' not defined (required by task ${requiredBy.name})." +
          s"We have these submitters: ${submitters.map(_.name).mkString(" ")}",
        requiredBy)
    }
  }
  
  private def getRunAction(submitterDef: SubmitterDef): ActionDef = {
    submitterDef.actions.find { action => action.name.toString == "run" } match {
      case Some(action: ActionDef) => action
      case None => throw new FileFormatException(s"No 'run' action defined for submitter ${submitterDef.name}",
                     submitterDef)
    }
  }
  
  def getSubmitter(task: RealTask): SubmitterDef = {
      val allDotParams: Seq[ResolvedLiteralSpec] = task.paramVals.filter(_.origSpec.dotVariable)
      allDotParams.find { p: ResolvedLiteralSpec => p.origSpec.name == "submitter" } match {
        case Some(p) => getSubmitter(p)
        case None => getDefaultSubmitter("shell", task.taskDef)
      }
    }

  def run(taskEnv: FullTaskEnvironment) {
    val submitterDef: SubmitterDef = getSubmitter(taskEnv.task)
    val requiredParams: Set[String] = submitterDef.params.map(_.name).toSet
    // only include the dot params from the task that are explicitly requested by the submitter
    val allDotParams: Seq[ResolvedLiteralSpec] = taskEnv.task.paramVals.filter(_.origSpec.dotVariable)
    val dotParamsForSubmitter: Seq[ResolvedLiteralSpec] = allDotParams.filter { litSpec: ResolvedLiteralSpec =>
      requiredParams.contains(litSpec.origSpec.name)
    }
    val dotParamsEnv: Seq[(String,String)] = dotParamsForSubmitter.map { p => (p.origSpec.name, p.srcSpec.rval.value) }
    debug(s"Dot parameters going into environment for run action are: ${dotParamsEnv}")
    val runAction = getRunAction(submitterDef)

    // note that run requires the entire environment from the original task
    // since we might not be doing any wrapping at all
    val env: Seq[(String,String)] = Seq(
          ("CONFIGURATION", taskEnv.dirs.confName.getOrElse("")),
          ("TASK", taskEnv.task.name),
          ("REALIZATION", taskEnv.task.realization.toString),
          ("TASK_VARIABLES", taskEnv.taskVariables),
          ("COMMANDS", taskEnv.task.commands.toString)) ++
        dotParamsEnv ++ taskEnv.env
        
    // To prevent some strange quoting bugs, treat COMMANDS specially and directly substitute it
    val code = runAction.commands.toString
    
    // TODO: Escape any double quotes?
    val QUOT = "\""
    val taskScript = Seq("# This script will try to run a task *outside* any specified submitter") ++
                     Seq("# Note: This script is for archival; it is not actually run by ducttape") ++
                     taskEnv.env.map { case (key,value) => s"export ${key}=${QUOT}${value}${QUOT}" } ++
                     Seq(taskEnv.task.commands.toString)
    Files.write(taskScript, taskEnv.taskScriptFile)

    debug(s"Execution environment is: ${env}")

    System.err.println(s"Using submitter ${submitterDef.name}")
    val stdPrefix = s"${taskEnv.task.name}/${taskEnv.task.realization}"
    val exitCode = Shell.run(code, stdPrefix, taskEnv.where, env, taskEnv.stdoutFile, taskEnv.stderrFile)
    Files.write(s"${exitCode}", taskEnv.exitCodeFile)
    if (exitCode != 0) {
      throw new BashException(s"Task ${taskEnv.task} failed")
    }
  }
}
