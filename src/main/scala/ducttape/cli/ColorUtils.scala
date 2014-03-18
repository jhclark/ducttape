package ducttape.cli
import ducttape.workflow.RealTask
import ducttape.exec.DirectoryArchitect
import ducttape.workflow.Realization

object ColorUtils {
  def colorizeDir(taskName: String, real: Realization)
                 (implicit dirs: DirectoryArchitect): String = {
    val x = s"${dirs.confBaseDir.getAbsolutePath}/${Config.taskNameColor}${taskName}${Config.resetColor}"
    if (dirs.flat) {
      x
    } else {
      s"${x}/${Config.realNameColor}${real.toCanonicalString()}${Config.resetColor} (${Config.realFullNameColor}${real.toFullString(hashLongNames=false)}${Config.resetColor})"
    }
  }
  
  def colorizeDirs(list: Iterable[RealTask])
                  (implicit dirs: DirectoryArchitect): Seq[String] = {
    list.toSeq.map{ task => colorizeDir(task.name, task.realization) }
  }
}
