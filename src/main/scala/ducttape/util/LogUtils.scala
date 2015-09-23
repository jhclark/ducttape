// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.util

import java.io.FileInputStream
import java.io.File

object LogUtils {
  def initJavaLogging() {
    val userConfig = new File(Environment.UserHomeDir, ".ducttape.logging")
    val installConfig = new File(Environment.InstallDir, "logging.properties")
    
    val logConfig: Option[File] = {
      if (userConfig.exists) {
        Some(userConfig)
      } else if (installConfig.exists) {
        Some(installConfig)
      } else {
        None
      }
    }
    
    logConfig match {
      case Some(file) => java.util.logging.LogManager.getLogManager.readConfiguration(new FileInputStream(file))
      case None => ;
    }
  }
}
