package ducttape.workflow

import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.TaskDef
import ducttape.util.BranchPrefixTreeMap

// delay some parts of resolution (choosing branches) for unpack-time
// here, we can collapse some of the original information encoded in nested
// branch points into the TreeMap: Even though we may re-sort the branches
// in a way that's no longer consistent with the original tree structure of the nesting,
// this information is preserved by the MetaHyperDAG -- all we have to do is match
// the unordered set of branches provided by the UnpackedMetaHyperDAG walker with
// which parent specs those branches are associated with
// NOTE: The Trie keeps the branches ordered for the purposes of efficiency only
class ResolvableSpecType[SpecT <: Spec](val mySpec: Spec,
                                        val branchMap: BranchPrefixTreeMap[(SpecT,Option[TaskDef])])

// TaskTemplate is responsible for turning a Resolvable into a Resolved
class ResolvedSpecType[SpecT <: Spec](val mySpec: Spec,
                                      val srcSpec: SpecT,
                                      val srcTaskDef: Option[TaskDef],
                                      val srcReal: Realization)

object SpecTypes {
  type ResolvedSpec = ResolvedSpecType[Spec]
  type ResolvedLiteralSpec = ResolvedSpecType[LiteralSpec]
}
import SpecTypes._