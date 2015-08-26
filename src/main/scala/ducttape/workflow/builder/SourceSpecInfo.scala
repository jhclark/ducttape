// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow.builder

import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.workflow.Branch

private[builder] class SourceSpecInfo(
                          val srcSpec: Spec,
                          val srcTask: Option[TaskDef],
                          val grafts:  Seq[Branch]) {
  
  def withUpdatedGrafts(newGrafts: Seq[Branch]) : SourceSpecInfo = { 
    return new SourceSpecInfo(srcSpec, srcTask, newGrafts)
  }
}
                                     
