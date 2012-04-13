package ducttape.util
import java.io.File

object Environment {
  def getJarFile = new File(Environment.getClass.getProtectionDomain.getCodeSource.getLocation.getPath)
  def getJarDir = getJarFile.getParentFile
  
  import java.lang.management._
  // vmName looks like 11411@mymachine.org
  lazy val vmName = ManagementFactory.getRuntimeMXBean.getName
  lazy val pid: Int = vmName.split("@")(0).toInt
  lazy val hostname: String = vmName.split("@")(1)
}