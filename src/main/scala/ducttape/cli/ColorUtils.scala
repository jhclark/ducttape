package ducttape.cli
import ducttape.workflow.RealTask
import ducttape.exec.DirectoryArchitect
import ducttape.workflow.Realization

object ColorUtils {
  def colorizeDir(taskName: String, real: Realization)
                 (implicit dirs: DirectoryArchitect): String = {
    val x = "%s/%s%s%s".format(dirs.confBaseDir.getAbsolutePath, Config.taskNameColor, taskName, Config.resetColor)           
    if (dirs.flat) {
      x
    } else {
      x + "/%s%s%s".format(Config.realNameColor, real.toString, Config.resetColor)
    }
  }
  
  def colorizeDirs(list: Iterable[RealTask])
                  (implicit dirs: DirectoryArchitect): Seq[String] = {
    list.toSeq.map{ task => colorizeDir(task.name, task.realization) }
  }
}