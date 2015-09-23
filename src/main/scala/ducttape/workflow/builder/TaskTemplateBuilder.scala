// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow.builder

import ducttape.syntax.Namespace
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.AbstractSyntaxTree.WorkflowDefinition
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPoint
import ducttape.workflow.BranchPointFactory
import ducttape.workflow.Task
import ducttape.workflow.TaskTemplate
import ducttape.workflow.SpecTypes.SpecPair
import ducttape.workflow.SpecTypes.LiteralSpecPair

import collection.Map
import collection.mutable
//
import grizzled.slf4j.Logging

// unlike WorkflowBuilder, we have no interaction with hyperdag framework here
private[builder] class TaskTemplateBuilder(
    wd: WorkflowDefinition,
    private[builder] val confSpecs: Map[String,Spec],
    private[builder] val branchPointFactory: BranchPointFactory,
    private[builder] val branchFactory: BranchFactory) extends Logging {
  
  def findTasks(): FoundTasks = {
    
    val tasks: Seq[TaskDef] = 
      // Concatenate regular task definitions
      // with task definitions constructed via function calls
      (wd.tasks ++ wd.functionCallTasks)

    val taskMap: Map[Namespace,TaskDef] = {
      tasks.
      // Make each element in the list a tuple, 
      // where the first element is the task name
      // and the second element is the TaskDef object (that is, an AST node)
      map { t: TaskDef => (t.name, t) }.
      // then convert this list of tuples into a map
      // where each TaskDef can be retrieved via its name      
      toMap
    }
              
    val branchPoints = new mutable.ArrayBuffer[BranchPoint]
    val parents: Map[TaskTemplate,BranchPointTreeGrafts] = tasks.map { taskDef: TaskDef =>
      val tree = new BranchPointTree(Task.NO_BRANCH_POINT)
      val treeData = new BranchPointTreeGrafts(tree, Nil)
      val baselineTree = new BranchTree(Task.NO_BRANCH)
      tree.children += baselineTree

      // Some methods in VariableHandler and BranchPointHandler
      //   need access to this object's member variables.
      //
      // But, those methods already take an obscene number of parameters
      // 
      // So, we make this object implicit.
      //
      // This essentially allows us to pass this object as a parameter to various methods
      //   without actually explicitly listing it in the method call
      implicit val taskTemplateBuilder = this
            
      // parameters are different than file dependencies in that they do not
      // add any temporal dependencies between tasks and therefore do not
      // add any edges in the MetaHyperDAG
      //
      // params have no effect on temporal ordering, but can affect derivation of branches
      // therefore, params are *always* rooted at phantom vertices, no matter what -- hence, we store None
      //
      // most of the heavy lifting in this method is done by resolveBranchPoint(), which determines which
      // source specs should be used for each of the input/parameter specs in this task. these are then
      // added at the correct position in the BranchPointTree
      for (paramSpec: Spec <- taskDef.params) {
        BranchPointHandler.resolveBranchPoint(taskDef, paramSpec, taskMap, isParam=true)(
          baselineTree, Seq(Task.NO_BRANCH), Some(taskDef), paramSpec, Nil)(resolveVarFunc=VariableHandler.resolveParam)
      }

      for (inSpec: Spec <- taskDef.inputs) {
        BranchPointHandler.resolveBranchPoint(taskDef, inSpec, taskMap, isParam=false)(
          baselineTree, Seq(Task.NO_BRANCH), Some(taskDef), inSpec, Nil)(resolveVarFunc=VariableHandler.resolveInput)
      }
            
      val paramVals: Seq[LiteralSpecPair] = tree.specs.filter(_.isParam).map { spec =>
        val literalSrcSpec = spec.srcSpec.asInstanceOf[LiteralSpec] // guaranteed to succeed since isParam
        new LiteralSpecPair(spec.origSpec, spec.srcTask, literalSrcSpec, isParam=true)
      }.toSeq

      val inputVals: Seq[SpecPair] = tree.specs.filter(_.isInput).map { spec =>
        new SpecPair(spec.origSpec, spec.srcTask, spec.srcSpec, isParam=false)
      }.toSeq

      val taskT = new TaskTemplate(taskDef, inputVals, paramVals)
      (taskT, treeData) // key, value for parents map
    }.toMap
    
    val taskTemplates: Seq[TaskTemplate] = parents.keys.toSeq
    new FoundTasks(taskTemplates, parents, branchPoints)
  }
  
}
