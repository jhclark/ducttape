package ducttape.exec

import java.io.File

import collection._

import ducttape.syntax.FileFormatException
import ducttape.syntax.AbstractSyntaxTree._
import ducttape.workflow.Branch
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.workflow.SpecTypes.ResolvedSpec
import ducttape.util.Files

import grizzled.slf4j.Logging

class InputChecker(dirs: DirectoryArchitect) extends UnpackedDagVisitor with Logging {

  val errors = new mutable.ArrayBuffer[FileFormatException]

  override def visit(task: RealTask) {
    for (inSpec: ResolvedSpec <- task.inputVals) {
      inSpec.srcTask match {
        case Some(_) => ; // input will be generated during workflow execution
        case None =>
          inSpec.srcSpec.rval match {
            case Literal(path) => {
              // detect and handle globs
              val literalSpec = inSpec.srcSpec.asInstanceOf[LiteralSpec]
              val fileGlob: File = dirs.resolveLiteralPath(literalSpec)
              val globbedFiles: Seq[File] = Files.glob(fileGlob.getAbsolutePath)
              for (file <- globbedFiles) {
                debug("Checking for file %s".format(file))
                if (!file.exists) {
                  // TODO: Not FileFormatException
                  errors += new FileFormatException("Input file not found: %s required at %s:%d, defined at %s:%d".
                              format(
                                file.getAbsolutePath,
                                inSpec.origSpec.declaringFile, inSpec.origSpec.pos.line,
                                inSpec.srcSpec.declaringFile, inSpec.srcSpec.pos.line),
                              List(inSpec.srcSpec, inSpec.origSpec))
                }
              }
            }
            case _ => throw new RuntimeException("Expected source file to be a literal")
        }
        case _ => ;
      }
    }
  }
}
