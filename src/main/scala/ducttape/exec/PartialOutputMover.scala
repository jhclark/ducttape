package ducttape.exec

import java.io.File
import collection._
import ducttape.workflow.Realization
import ducttape.util.Files
import ducttape.util.OrderedSet
import ducttape.util.MutableOrderedSet
import ducttape.workflow.RealTask
import ducttape.versioner.TaskVersion

/**
 * Moves incomplete or invalidated tasks to the attic before re-running them.
 * partial is a list of the task/realizations that have partial output that needs to be moved
 *         (and are guaranteed to have version numbers)
 * broken is a list of the task/realizations that are partial but are missing information
 *        such as a version number and so are considered broken. these will be deleted
 */
class PartialOutputMover(dirs: DirectoryArchitect,
                         partial: Set[(String,Realization)],
                         broken: Set[(String,Realization)]) extends UnpackedDagVisitor {
  
  override def visit(task: RealTask) {
    
    System.err.println("Considering %s".format(task))
    
    if (partial( (task.name, task.realization) )) {
      val taskEnv = new TaskEnvironment(dirs, task)
      val version = TaskVersion.read(taskEnv.versionFile)
      
      System.err.println("Moving %s to the attic".format(task))
      val origDir = dirs.assignDir(task)
      val atticDir = dirs.assignAtticDir(task, version)
      Files.moveDir(origDir, atticDir)
    }
    
    if (broken( (task.name, task.realization) )) {
      System.err.println("Removing broken partial output for %s".format(task))
      val origDir = dirs.assignDir(task)
      Files.deleteDir(origDir)
    }
  }
}
