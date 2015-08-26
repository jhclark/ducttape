// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.cli

import java.io.File
import ducttape.util.BashException
import ducttape.util.DucttapeException
import ducttape.util.Files
import ducttape.syntax.FileFormatException
import java.util.concurrent.ExecutionException
import grizzled.slf4j.Logging

object ErrorUtils extends Logging {
  
  def prettyPrintError(e: FileFormatException, prefix: String, color: String) {
    debug(e.getMessage)
    debug(e.getStackTraceString)
      
    System.err.println("%s%s: %s%s".format(color, prefix, e.getMessage, Config.resetColor))
    for ( (file: File, line: Int, col: Int, untilLine: Int) <- e.refs) {
      System.err.println("%s%s:%d%s".format(Config.errorLineColor, file.getAbsolutePath, line, Config.resetColor))
      val badLines = Files.read(file).drop(line-1).take(line-untilLine+1)
      System.err.println(Config.errorScriptColor + badLines.mkString("\n"))
      System.err.println(" " * (col-2) + "^")
    }
  }
      
  // format exceptions as nice error messages
  def ex2err[T](func: => T) : T = {
    
    def exitError(e: Exception): T = {
      debug(e.getMessage)
      debug(e.getStackTraceString)
      
      System.err.println("%sERROR: %s".format(Config.errorColor, e.getMessage))
      System.exit(1)
      throw new Error("Unreachable") // make the compiler happy
    }
    
    def catcher(e: Throwable): T = {
      e match {
        case e: ExecutionException => catcher(e.getCause)
        case e: BashException => exitError(e)
        case e: DucttapeException => exitError(e)
        case _ => ;
      }
      
      debug(e.getMessage)
      debug(e.getStackTraceString)
      
      e match {
        case e: FileFormatException => {
          prettyPrintError(e, prefix="ERROR", color=Config.errorColor)
          System.exit(1)
          throw new Error("Unreachable") // make the compiler happy
        }
        case t: Throwable => throw t
      }
    }
    
    try { func } catch {
      case e: Throwable => catcher(e)
    }
  }
}
