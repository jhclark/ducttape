package ducttape.util

import java.io.FileInputStream
import java.io.File

object LogUtils {
  def initJavaLogging() {
    try {
     val logConfig = new File(Environment.getJarDir, "logging.properties")
     java.util.logging.LogManager.getLogManager.readConfiguration(new FileInputStream(logConfig))
//   System.err.println("Initialized Java logging system from logging.properties");            
    } catch {
      case _ => System.err.println("Did not initialize Java logging system from logging.properties");
    }
  }
}