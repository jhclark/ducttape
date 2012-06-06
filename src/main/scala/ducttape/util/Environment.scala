package ducttape.util
import java.io.File
import scala.annotation.tailrec

object Environment {
  val PWD = new File(".")
  def getJarFile = new File(Environment.getClass.getProtectionDomain.getCodeSource.getLocation.getPath)
  def getJarDir = getJarFile.getParentFile
  
  def hasTTY: Boolean = java.lang.System.console != null
  
  def getAllThreads: Iterable[Thread] = {
    @tailrec def getRootGroup(tg: ThreadGroup): ThreadGroup = {
      if (tg.getParent == null) tg else getRootGroup(tg.getParent)
    }
    val root = getRootGroup(Thread.currentThread.getThreadGroup)
    val estSize = root.activeCount * 2
    val arr = new Array[Thread](estSize)
    root.enumerate(arr, true)
    arr.toSeq.filter(_ != null)
  }
  
  
  import java.lang.management._
  // vmName looks like 11411@mymachine.org
  lazy val vmName = ManagementFactory.getRuntimeMXBean.getName
  lazy val pid: Int = vmName.split("@")(0).toInt
  lazy val hostname: String = vmName.split("@")(1)
}