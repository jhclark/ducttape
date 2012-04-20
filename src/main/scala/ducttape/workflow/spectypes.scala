package ducttape.workflow

import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.TaskDef

class ResolvedSpecType[SpecT <: Spec](val mySpec: Spec,
                              val srcSpec: SpecT,
                              val srcTaskDef: TaskDef,
                              val srcReal: Realization);
object SpecTypes {
  type ResolvedSpec = ResolvedSpecType[Spec];
  type ResolvedLiteralSpec = ResolvedSpecType[LiteralSpec];
}
import SpecTypes._