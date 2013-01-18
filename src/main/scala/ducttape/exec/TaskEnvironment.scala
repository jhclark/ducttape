package ducttape.exec

import ducttape.workflow.RealTask
import ducttape.workflow.Realization
import java.io.File
import grizzled.slf4j.Logging

/**
 * Unlike the FullTaskEnvironment, does not require knowledge of packageVersions,
 * but does not provide full list of environment variables
 *
 * Input/Output files in TaskEnvironment are guaranteed to have been normalized
 * via DirectoryArchitect (e.g. tildes become the user's home directory)
 */
class TaskEnvironment(val dirs: DirectoryArchitect,
                      val task: RealTask) extends Logging {
  
  val inputs: Seq[(String, String)] = for (inputVal <- task.inputVals) yield {
    val inFile = dirs.getInFile(inputVal.origSpec, task.realization,
                                inputVal.srcSpec, inputVal.srcTask, inputVal.srcReal)
    debug(s"For inSpec ${inputVal.origSpec} with srcSpec ${inputVal.srcSpec} and " +
          s"srcReal ${inputVal.srcReal} with parent task ${inputVal.srcTask}, " +
          s"got path: ${inFile.getAbsolutePath}")
    (inputVal.origSpec.name, inFile.getAbsolutePath)
  }
    
  // set param values (no need to know source active branches since we already resolved the literal)
  val params: Seq[(String,String)] = for (paramVal <- task.paramVals) yield {
    debug(s"For paramSpec ${paramVal.origSpec} with srcSpec ${paramVal.srcSpec}, got value: " +
          s"${paramVal.srcSpec.rval.value}")
    (paramVal.origSpec.name, paramVal.srcSpec.rval.value)
  }

  // assign output paths
  val outputs: Seq[(String, String)] = for (outSpec <- task.outputs) yield {
    val outFile = dirs.assignOutFile(outSpec, task.taskDef, task.realization)
    debug(s"For outSpec ${outSpec} got path: ${outFile}")
    (outSpec.name, outFile.getAbsolutePath)
  }
  
  val where = dirs.assignDir(task)
  val stdoutFile = new File(where, "ducttape_stdout.txt")
  val stderrFile = new File(where, "ducttape_stderr.txt")
  val exitCodeFile = new File(where, "ducttape_exit_code.txt")
  val versionFile = new File(where, "ducttape_version.txt")

  // TODO: XXX: Do we still need this file?
  val invalidatedFile = new File(where, "ducttape.INVALIDATED")

  // the lock file *must* be placed outside of the where directory
  // since we must sequentially acquire the lock and *then* atomically move
  // the where directory to the attic (including its exact inode, in case
  // any running processes still have that inode open)
  val lockFile = new File(where.getParentFile, s"${where.getName}.LOCK")
  
  // the full symlink is to be for user-friendly navigation of the directory tree
  // NOT for internal use by ducttape
  lazy val fullSymlink = dirs.assignLongSymlink(task)
}

/**
 * Includes all environment variables, but requires knowledge of packgeVersions
 */
class FullTaskEnvironment(dirs: DirectoryArchitect,
                          val packageVersions: PackageVersioner,
                          task: RealTask) extends TaskEnvironment(dirs, task) {
  val packageNames: Seq[String] = task.packages.map(_.name)
  val packageBuilds: Seq[BuildEnvironment] = {
    packageNames.map { name =>
      val packageVersion = packageVersions(name)
      new BuildEnvironment(dirs, packageVersion, name)
    }
  }
  val packageEnvs: Seq[(String,String)] = {
    packageBuilds.map(build => (build.packageName, build.buildDir.getAbsolutePath) )
  }

  lazy val env = inputs ++ outputs ++ params ++ packageEnvs
  lazy val taskVariables = env.map { case (key, value) => s"${key}=${value}" }.mkString("\n")
}
