// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.util
import java.io.File
import scala.annotation.tailrec

// note: do *not* use the logging framework here since it won't have been intialized yet for
// some of these values
object Environment {
  var Debug = false

  val PWD = new File(".")
  def getJarFile = new File(Environment.getClass.getProtectionDomain.getCodeSource.getLocation.getPath)
  def getJarDir = getJarFile.getParentFile

  val UserHomeDir: File = new File(System.getProperty("user.home"))

  val InstallDir: File = {
    val jarDir = Environment.getJarDir
    // detect system-wide installs
    if (jarDir.getName == "bin") {
      // e.g. /usr/local/share/ducttape
      val shareDir = new File(jarDir.getParentFile, "share/ducttape")
      if (Debug)
        System.err.println("Detected a system-wide install. Trying to use share directory: %s".format(shareDir.getAbsolutePath))
      if (shareDir.exists) {
        shareDir
      } else {
        val parentDir = jarDir.getParentFile
        if (Debug) {
          System.err.println("System-wide ducttape share directory not found: %s\nUsing %s".format(shareDir.getAbsolutePath,parentDir.getAbsolutePath))
        } 
        parentDir
      }
    } else {
      if (Debug)
        System.err.println("Not a system-wide installation. Using JAR directory as install directory: %s".format(jarDir.getAbsolutePath))
      jarDir
    }
  }
  
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
