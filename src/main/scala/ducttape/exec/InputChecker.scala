package ducttape.exec

import java.io.File

import collection._

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.workflow.Branch
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.workflow.SpecTypes.ResolvedSpec
import ducttape.util.Files

class InputChecker(dirs: DirectoryArchitect) extends UnpackedDagVisitor {

  // TODO: Make this a list of exceptions that take ASTTypes
  val errors = new mutable.ArrayBuffer[String]

  override def visit(task: RealTask) {
    for (inSpec: ResolvedSpec <- task.inputVals) {
      inSpec.srcTask match {
        case Some(_) => ; // input will be generated during workflow execution
        case None =>
          inSpec.srcSpec.rval match {
            case Literal(path) => {
              val file = dirs.resolveLiteralPath(path)
              if (!file.exists) {
                // TODO: Be specific about which file
                errors += "Input file not found: %s required at %s:%d, defined at %s:%d".format(
                             file.getAbsolutePath,
                             inSpec.origSpec.declaringFile, inSpec.origSpec.pos.line,
                             inSpec.srcSpec.declaringFile, inSpec.srcSpec.pos.line)
              }
            }
            case _ => throw new RuntimeException("Expected source file to be a literal")
        }
        case _ => ;
      }
    }
  }
}
