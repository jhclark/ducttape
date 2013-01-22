package ducttape.workflow.builder

import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.syntax.Namespace
import ducttape.workflow.Branch
import ducttape.workflow.BranchFactory
import ducttape.workflow.BranchPointFactory

import collection.Map

//class State[SpecT <: Spec](
class State(

//             val confSpecs:          Map[String,Spec],
//             val branchPointFactory: BranchPointFactory,
//             val branchFactory:      BranchFactory,

             val taskDef:            TaskDef,
             val origSpec:           Spec,
             val taskMap:            Map[Namespace,TaskDef],
             val isParam:            Boolean,

             val prevTree:           BranchTree,      // sometimes this changes
             val branchHistory:      Seq[Branch],     // sometimes this changes
             val curTask:            Option[TaskDef], // sometimes this changes
             val curSpec:            Spec,            // sometimes this changes
             val prevGrafts:         Seq[Branch],     // sometimes this changes

             val resolveVarFunc:     (TaskDef, Map[Namespace,TaskDef], Spec, Option[TaskDef]) => SourceSpecInfo             
//             val resolveVarFunc:     (TaskDef, Map[Namespace,TaskDef], Spec, Option[TaskDef]) => (SpecT, Option[TaskDef], Seq[Branch])
                          
            )