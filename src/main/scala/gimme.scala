package ducttape

import java.io.File

object Gimme {

  def getPackagesFromParams(params: Seq[(String,String)]): Seq[String] = {
    params.filter{case (k:String, v:String) => k == ".gimme"}.map(_._2)
  }

  def getCommand(workflowDir: File, packageName: String): String = {
    val gimmeDir = new File(workflowDir, "gimme")
    val gimmeScript = new File(gimmeDir, packageName)
    if(!gimmeScript.exists) {
      throw new RuntimeException("Gimme script for package %s not found at %s".format(packageName, gimmeScript.getAbsolutePath))
    }
    "%s gimme %s".format(gimmeScript.getAbsolutePath, packageName)
  }

}
