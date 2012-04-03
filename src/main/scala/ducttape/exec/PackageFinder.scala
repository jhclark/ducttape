package ducttape.exec

import collection._
import ducttape.Config
import ducttape.versioner.WorkflowVersioner
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.syntax.AbstractSyntaxTree.PackageDef
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.FileFormatException
import java.io.File

// dirs and versions are unimportant other than being required to generate the TaskEnvironment
class PackageFinder(conf: Config,
                    dirs: DirectoryArchitect,
                    versions: WorkflowVersioner,
                    todo: Set[(String,Realization)],
                    packageDefs: Map[String,PackageDef]) extends UnpackedDagVisitor {
  
  val packages = new mutable.HashSet[PackageDef]
  
  override def visit(task: RealTask) {
    if(todo((task.name, task.realization))) {
      for(packageSpec: Spec <- task.packages) {
        if(packageDefs.contains(packageSpec.name)) {
          packages += packageDefs(packageSpec.name)
        } else {
          throw new FileFormatException(
            "Undefined package %s".format(packageSpec.name),
            List( (new File("unknown"), packageSpec.pos, packageSpec.pos.line) )) // TODO: XXX: filename
        }
      }
    }
  }
}