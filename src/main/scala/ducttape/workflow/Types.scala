// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.workflow

import collection._
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.util.MultiSet
import ducttape.hyperdag.PackedVertex
import ducttape.hyperdag.meta.UnpackedMetaVertex
import ducttape.hyperdag.HyperEdge
import ducttape.hyperdag.meta.MetaEdge
import ducttape.hyperdag.meta.UnpackedChainedMetaVertex
import ducttape.workflow.SpecTypes.SpecPair

object Types {
  class HashMultiMap[A,B] extends mutable.HashMap[A,mutable.Set[B]] with mutable.MultiMap[A,B];

  /** when walking an unpacked DAG, only allow the same branch to appear in a realization once
   * By using Scala's fast immutable sets, we save big on space complexity while keeping reasonable time
   * See Phil Bagwell's work at EPFL on data sharing in immutable/persistent data structures
   *
   * The unpack state is used by the RealizationMunger (see
   * [[ducttape.hyperdag.walker.UnpackedPhantomMetaDagWalker]] for more).
   * For example usages, see [[ducttape.workflow.GlobalBranchPointConstraint]]
   * and [[ducttape.workflow.BranchGraftMunger]]
   *
   * edgeState is essentially used as a staging area for branches we're considering
   * adding to the realization. We keep them in the staging area first because munging
   * can modify the edgeState before it finally needs to be merged with the hyperedgeState.
   * For example, if an incoming edge has BP1.b1, which is then matched in the edgeState by a graft
   * expecting BP1.b1 the graft munger will then remove the branch BP1.b1 from the
   * edgeState. Finally, the global branch point constraint will check that no branch
   * in the edgeState conflicts with an already-selected branch in the hyperedgeState.
   * Notice that multiple edge-only actions had to take place before the final result
   * could be incorporated into the hyperedgeState.
   *
   * See also [[ducttape.workflow.HyperWorkflow.HyperWorkflowStateMunger]] */ 
  class UnpackState(
      // hyperedgeState: The branches we've already committed to adding during
      //   the current hyperedge traversal
      // we must know this to enforce [[ducttape.workflow.GlobalBranchPointConstraint]]
      val hyperedgeState: immutable.Map[BranchPoint, Branch],
      // edgeState: The branches from a particular edge that we're considering
      //   but might still get munged via grafting or rejected due to constraints
      val edgeState: immutable.Map[BranchPoint, Branch]) {
    override def toString() = "he=%s++e=%s".format(hyperedgeState.values.mkString("-"), edgeState.values.mkString("-"))
  }

  object UnpackState {
    val emptyMap = new immutable.HashMap[BranchPoint, Branch]
    val empty = new UnpackState(emptyMap, emptyMap)
  }
  
  // this is an Option[TaskTemplate] due to the constraints of
  // [[ducttape.hyperdag.meta.PhantomMetaHyperDag]]
  type PackedWorkVert = PackedVertex[Option[TaskTemplate]]

  /** See [[ducttape.hyperdag.meta.MetaHyperDag]] for definitions of generic types V,H,E
   *  and see [[ducttape.hyperdag.walker.UnpackedDagWalker for the definition of generic type D.
   * V = TaskTemplate
   * H = Branch
   * E = SpecGroup
   * D = Branch
   */
  type UnpackedWorkVert = UnpackedChainedMetaVertex[TaskTemplate,Branch,SpecGroup,Branch]
  type WorkflowEdge = HyperEdge[Branch,SpecGroup]
  type WorkflowMetaEdge = MetaEdge[BranchPoint, Branch, SpecGroup]
}
