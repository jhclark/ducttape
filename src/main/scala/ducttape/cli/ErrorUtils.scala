package ducttape.cli

import java.io.File
import ducttape.util.BashException
import ducttape.util.DucttapeException
import ducttape.util.Files
import ducttape.syntax.FileFormatException
import java.util.concurrent.ExecutionException

object ErrorUtils {
  // format exceptions as nice error messages
  def ex2err[T](func: => T)(implicit conf: Config): T = {
    
    def exitError(e: Exception): T = {
      System.err.println("%sERROR: %s".format(conf.errorColor, e.getMessage))
      System.exit(1)
      throw new Error("Unreachable") // make the compiler happy
    }
    
    def catcher(e: Throwable): T = {
      e match {
        case e: ExecutionException => catcher(e.getCause)
        case e: FileFormatException => {
          System.err.println("%sERROR: %s%s".format(conf.errorColor, e.getMessage, conf.resetColor))
          for ( (file: File, line: Int, col: Int, untilLine: Int) <- e.refs) {
            System.err.println("%s%s:%d%s".format(conf.errorLineColor, file.getAbsolutePath, line, conf.resetColor))
            val badLines = Files.read(file).drop(line-1).take(line-untilLine+1)
            System.err.println(conf.errorScriptColor + badLines.mkString("\n"))
            System.err.println(" " * (col-2) + "^")
          }
          System.exit(1)
          throw new Error("Unreachable") // make the compiler happy
        }
        case e: BashException => exitError(e)
        case e: DucttapeException => exitError(e)
        case t: Throwable => throw t
      }
    }
    
    try { func } catch {
      case e => catcher(e)
    }
  }
}