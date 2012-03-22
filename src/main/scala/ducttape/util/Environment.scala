package ducttape.util
import java.io.File

object Environment {
  def getJarFile = new File(Environment.getClass.getProtectionDomain.getCodeSource.getLocation.getPath)
  def getJarDir = getJarFile.getParentFile
}