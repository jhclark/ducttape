package ducttape.exec

import collection._
import ducttape.workflow.Realization
import ducttape.workflow.RealTask
import ducttape.syntax.Namespace
import ducttape.syntax.AbstractSyntaxTree.PackageDef
import ducttape.syntax.AbstractSyntaxTree.Spec
import ducttape.syntax.FileFormatException
import java.io.File

// Visits each planned task, discovering what software packages
// will be required to execute it 
class PackageFinder(todo: Option[Set[(String,Realization)]],
                    packageDefs: Map[Namespace,PackageDef]) extends UnpackedRealDagVisitor {
  
  val packages = new mutable.HashSet[PackageDef]
  
  override def visit(task: RealTask) {
    // TODO: Why do we need todo here? Isn't this enforced by the walker?
    if (todo == None || todo.get( (task.name, task.realization) )) {
      for (packageSpec: Spec <- task.packages) {
        // TODO: XXX: HACK: Lane: This may not handle namespaces correctly
        val packageNamespace = Namespace.fromString(packageSpec.name)
        if (packageDefs.contains(packageNamespace)) {
          packages += packageDefs(packageNamespace)
        } else {
          // TODO: This should be checked by now...
          throw new FileFormatException(s"Undefined package ${packageSpec.name}", packageSpec)
        }
      }
    }
  }
}
