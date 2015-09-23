// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import ducttape.syntax.Namespace
import ducttape.workflow.RealTask
import ducttape.workflow.VersionedTask
import ducttape.workflow.Realization
import ducttape.workflow.SpecTypes._
import ducttape.util.GlobValuesMap

import java.io.File
import grizzled.slf4j.Logging

/**
 * TaskEnvironment prepares all of the paths and environment variables needed
 * to run or work with a task.
 * 
 * Unlike the FullTaskEnvironment, does not require knowledge of packageVersions,
 * but does not provide full list of environment variables
 *
 * Input/Output files in TaskEnvironment are guaranteed to have been normalized
 * via DirectoryArchitect (e.g. tildes become the user's home directory)
 */
class TaskEnvironment(val dirs: DirectoryArchitect,
                      val task: VersionedTask) extends Logging {
  
  val inputs: Seq[(String,String)] = {

    val map = new GlobValuesMap

    for (inputVal: VersionedSpec <- task.inputValVersions) { //yield {
      val inFile = dirs.getInFile(inputVal.origSpec, task.realization,
                                  inputVal.srcSpec, inputVal.srcTask, inputVal.srcReal, inputVal.srcVersion)
      debug(s"For inSpec ${inputVal.origSpec} with srcSpec ${inputVal.srcSpec} @ " +
            s"${inputVal.srcTask}/${inputVal.srcReal}/${inputVal.srcVersion}: ${inFile.getAbsolutePath}")      
      map.addValue(inputVal.origSpec.name, inFile.getAbsolutePath)
    }   
    
    map.toStringTupleSeq    
  } 
  
  // set param values (no need to know source active branches since we already resolved the literal)
  val params: Seq[(String,String)] = {
    
    val map = new GlobValuesMap
    
    for (paramVal <- task.paramVals) {
      debug(s"For paramSpec ${paramVal.origSpec} with srcSpec ${paramVal.srcSpec}, got value: " +
            s"${paramVal.srcSpec.rval.value}")            
      map.addValue(paramVal.origSpec.name, paramVal.srcSpec.rval.value)
    }
    
    map.toStringTupleSeq

  }
  
  // assign output paths
  val outputs: Seq[(String, String)] = for (outSpec <- task.outputs) yield {
    val outFile = dirs.assignOutFile(outSpec, task.taskDef, task.realization, task.version)
    debug(s"For outSpec ${outSpec} got path: ${outFile}")
    (outSpec.name, outFile.getAbsolutePath)
  }
  
  val where = dirs.assignDir(task)
  val taskScriptFile = new File(where, "ducttape_task.sh")
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

  override def toString() = task.toString
}

/**
 * Includes all environment variables, but requires knowledge of packgeVersions
 */
class FullTaskEnvironment(dirs: DirectoryArchitect,
                          val packageVersions: PackageVersioner,
                          task: VersionedTask) extends TaskEnvironment(dirs, task) {
  val packageNames: Seq[Namespace] = task.packages.map { spec => Namespace.fromString(spec.name) }
  val packageBuilds: Seq[BuildEnvironment] = {
    packageNames.map { name =>
      val packageVersion = packageVersions(name)
      new BuildEnvironment(dirs, packageVersion, name)
    }
  }
  val packageEnvs: Seq[(String,String)] = {
    // TODO: XXX: HACK: Lane: We need a syntax for accessing fully qualified package names
    // For now, we're just arbitrarily picking the package name to represent the whole shebang, which is silly
    packageBuilds.map { build =>
      val packageVariableName: String = build.packageName.name
      (packageVariableName, build.buildDir.getAbsolutePath)
    }
  }

  lazy val env: Seq[(String,String)] = inputs ++ outputs ++ params ++ packageEnvs
  lazy val taskVariables = env.map { case (key, value) => s"${key}=${value}" }.mkString("\n")

  override def toString() = task.toString
}
