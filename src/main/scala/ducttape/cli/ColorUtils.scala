package ducttape.cli
import ducttape.workflow.RealTask
import ducttape.exec.DirectoryArchitect
import ducttape.workflow.Realization

object ColorUtils {
  def colorizeDir(taskName: String, real: Realization)
                 (implicit dirs: DirectoryArchitect, conf: Config): String = {
    val x = "%s/%s%s%s".format(dirs.confBaseDir.getAbsolutePath, conf.taskNameColor, taskName, conf.resetColor)           
    if (dirs.flat) {
      x
    } else {
      x + "/%s%s%s".format(conf.realNameColor, real.toString, conf.resetColor)
    }
  }
  
  def colorizeDirs(list: Iterable[RealTask])
                  (implicit dirs: DirectoryArchitect, conf: Config): Seq[String] = {
    list.toSeq.map{ task => colorizeDir(task.name, task.realization) }
  }
}