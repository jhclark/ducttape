package ducttape

import java.io.File
import ducttape.workflow._
import ducttape.util._

object Submitter {

  // returns a sequence of commands that should be executed
  // to submit and then wait for the completion of a job
  //
  // this may optionally write temporary files to realDir
  // generally, unless this is a no-op submitter,
  // we expect this to return one command such as "qsub job-script.sh"
  def prepare(workflowDir: File,
              taskWhere: File,
              params: Seq[(String,String)],
              commands: Seq[String],
              taskName: String,
              realization: Realization
              ): Seq[String] = {

    // use "shell" as default if none specified
    val submitterName = params.filter{case (k:String, v:String) => k == ".submitter"}.
                               map{case (k:String, v:String) => v}.headOption.getOrElse("shell")

    // 1) Search for this submitter (assume shell if none specified)
    val submitterScript = findSubmitter(workflowDir, submitterName)
    
    // 2) Send resource parameters (those that start with dots)
    // as environment variables to this script
    val env: Seq[(String,String)] = (for( (k,v) <- params; if k.startsWith(".") ) yield {
      val name = k.substring(1) // remove leading dot
      ("RESOURCE_%s".format(name), v)
    }) ++ Seq( ("TASK_NAME", taskName), ("REALIZATION", realization.toString) )

    // 3) Invoke script as subprocess and receive lines of stdout back
    //    as the new commands
    implicit def file2str(f: File) = f.getAbsolutePath
    Shell.runGetOutputLinesNoShell(submitterScript, taskWhere, env, commands)
  }

  def findSubmitter(workflowDir: File,
                    submitterName: String): File = {

    // 1) Search workflow dir
    val localSubmitterDir = new File(workflowDir, "submitters")
    val localMatch = new File(localSubmitterDir, submitterName)
    if(localMatch.exists) {
      return localMatch
    }

    // 2) Search ducttape dir
    val globalSubmitterDir = new File(Environment.getJarDir, "submitters")
    val globalMatch = new File(globalSubmitterDir, submitterName)
    if(globalMatch.exists) {
      return globalMatch
    } else {
      // TODO: Is there a better choice of exception class here?
      throw new RuntimeException("No matching submitter found for '%s' in %s or %s ".format(submitterName, localSubmitterDir.getAbsolutePath, globalSubmitterDir.getAbsolutePath))
    }
  }
}
