// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.exec

import ducttape.syntax.Namespace
import java.io.File

class BuildEnvironment(val dirs: DirectoryArchitect, val packageVersion: String, val packageName: Namespace) {
  val buildDir = dirs.assignBuildDir(packageName, packageVersion)
  val buildStdoutFile = new File(buildDir, "build_stdout.txt")
  val buildStderrFile = new File(buildDir, "build_stderr.txt")  
  val buildExitCodeFile = new File(buildDir, "build_exit_code.txt")  
}
