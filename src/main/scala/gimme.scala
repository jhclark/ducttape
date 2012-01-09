package ducttape

import java.io.File

object Gimme {
  def augment(workflowDir: File, params: Seq[(String,String)], commands: Seq[String]): Seq[String] = {
    val gimmeDir = new File(workflowDir, "gimme")
    val gimmeCommands: Seq[String] = {
      for(packageName <- params.filter{case (k:String, v:String) => k == ".gimme"}.map(_._2)) yield {
        val gimmeScript = new File(gimmeDir, packageName)
        if(!gimmeScript.exists) {
          throw new RuntimeException("Gimme script for package %s not found at %s".format(packageName, gimmeScript.getAbsolutePath))
        }
        "%s gimme %s".format(gimmeScript.getAbsolutePath, packageName)
      }
    }
    gimmeCommands ++ commands
  }
}
