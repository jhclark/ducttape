package ducttape.workflow

import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.AbstractSyntaxTree.LiteralSpec
import ducttape.syntax.AbstractSyntaxTree.TaskDef

class SpecPairType[SpecT <: Spec](
    val origSpec: Spec,
    val srcTask: Option[TaskDef],
    val srcSpec: SpecT,
    val isParam: Boolean) {
  def isInput = !isParam
  override def toString() = {
    val t = srcTask.getOrElse("")
    "%s=%s@%s".format(origSpec.name, srcSpec.rval, t) 
  }
}
                                        
// TaskTemplate is responsible for turning a Resolvable into a Resolved
class ResolvedSpecType[SpecT <: Spec](val origSpec: Spec,
                                      val srcTask: Option[TaskDef],
                                      val srcSpec: SpecT,
                                      val srcReal: Realization) {
  def this(that: ResolvedSpecType[SpecT]) = this(that.origSpec, that.srcTask, that.srcSpec, that.srcReal)
  override def toString() = "%s => %s (%s)".format(origSpec.name, srcSpec, srcReal)
}

class VersionedSpecType[SpecT <: Spec](spec: ResolvedSpecType[SpecT], val srcVersion: Int)
  extends ResolvedSpecType[SpecT](spec)


object SpecTypes {
  type ResolvedSpec = ResolvedSpecType[Spec]
  type ResolvedLiteralSpec = ResolvedSpecType[LiteralSpec]
  type SpecPair = SpecPairType[Spec]
  type LiteralSpecPair = SpecPairType[LiteralSpec]

  // literal specs don't need versions since we always use the
  // current workflow's version
  // TODO: XXX: This means the md5 hashing mechanism must
  // detect and prompt to invalidate old parameter versions
  type VersionedSpec = VersionedSpecType[Spec]
}
import SpecTypes._

// NOTE: This is primarily how edges are visualized in the GraphViz representation
class SpecGroup(val specPairs: Seq[SpecPair], val grafts: Seq[Branch]) {
  def toString(withNewlines: Boolean) = {
    if (withNewlines) {
      "[%s]\n".format(grafts.mkString(",")) + specPairs.mkString("\n")
    } else {
      "(SpecGroup [%s] %s)".format(grafts.mkString(","), specPairs.mkString(","))
    }
  }
  override def toString() = toString(withNewlines=false)
}

object SpecGroup {
  val empty = new SpecGroup(Nil, Nil)
  def empty(grafts: Seq[Branch]) = new SpecGroup(Nil, grafts)
}

import SpecTypes._
