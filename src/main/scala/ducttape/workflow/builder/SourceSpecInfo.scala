package ducttape.workflow.builder

import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.Branch

private[builder] class SourceSpecInfo(
                          val srcSpec: Spec,
                          val srcTask: Option[TaskDef],
                          val grafts:  Seq[Branch]) {
  
  def withUpdatedGrafts(newGrafts: Seq[Branch]) : SourceSpecInfo = { 
    return new SourceSpecInfo(srcSpec,srcTask, newGrafts)
  }
  
}
                                     