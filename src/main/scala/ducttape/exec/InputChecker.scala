package ducttape.exec

import java.io.File

import collection._

import ducttape.Config
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.workflow.Branch
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.util.Files

class InputChecker(conf: Config, dirs: DirectoryArchitect) extends UnpackedDagVisitor {

  val errors = new mutable.ArrayBuffer[String]

  override def visit(task: RealTask) {
    val inSpecs = task.inputVals.map{ case (_: Spec, srcSpec: Spec, _: TaskDef, _: Seq[Branch]) => {
      srcSpec
    }}
    for(inSpec <- inSpecs) inSpec.rval match {
      case Literal(path) => {
        val file = dirs.resolveLiteralPath(path)
        if(!file.exists) {
          // TODO: Be specific about which file
          errors += "Input file not found: %s required at unknown file at line %d".format(file.getAbsolutePath, inSpec.pos.line)
        }
      }
      case _ => ;
    }
  }
}
