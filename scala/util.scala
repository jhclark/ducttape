package ducttape.util

import scala.sys.process._
import scala.io._
import java.io._

object Files {
  def write(str: String, file: File) = {
    val fw = new FileWriter(file)
    fw.write(str)
    fw.close()    
  }

  def writer(file: File) = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8")))
}

object IO {
  import java.io.ByteArrayInputStream
  import java.io.CharArrayReader
  import java.io.File
  import java.io.InputStream
  import java.io.InputStreamReader
  import java.io.Reader
  import java.io.StringReader
  import java.net.URI
  import java.net.URL
  import scala.io.BufferedSource
  import scala.io.Source

  def read(input: Any, encoding: String) = input match {
    case bytes: Array[Byte]    => new InputStreamReader(new ByteArrayInputStream(bytes),encoding)
    case chars: Array[Char]    => new CharArrayReader(chars)
    case c: Char               => new StringReader(""+c)
    case file: File            => Source.fromFile(file, encoding).reader
    case inStream: InputStream => Source.fromInputStream(inStream,encoding).reader
    case string: String        => new StringReader(string)
    case uri: URI              => Source.fromFile(uri,encoding).reader
    case url: URL              => Source.fromURL(url,encoding).reader
    case any: AnyRef           => throw new RuntimeException("I don't know how to parse objects of type " + any.getClass())
    case _                    => throw new RuntimeException("I don't know how to parse objects of that type")
  }
}

object Shell {
  def run(cmds: Seq[String],
          workDir: File,
          env: Seq[(String,String)],
          stdoutFile: File,
          stderrFile: File) {
    run(cmds.mkString("\n"), workDir, env, stdoutFile, stderrFile)
  }

  def run(cmd: String,
          workDir: File,
          env: Seq[(String,String)],
          stdoutFile: File,
          stderrFile: File) {

    val stdout = Files.writer(stdoutFile)
    val stderr = Files.writer(stderrFile)
    def provideIn(x: OutputStream) = {
      val bash = new PrintStream(x)
      // TODO: Set environment here to be consistent with dry run script generation?
      bash.println("set -eo pipefail")
      bash.println(cmd)
      bash.close()
    }
    def handleOut(x: InputStream) = { for(line <- Source.fromInputStream(x).getLines()) {
      println(line)
      stdout.print(line) // no flush like println
      stdout.append('\n')
    }}
    def handleErr(x: InputStream) = { for(line <- Source.fromInputStream(x).getLines()) {
      System.err.println(line)
      stderr.print(line)
      stderr.append('\n')
    }}
    // pass env as varargs
    val code = Process("bash", workDir, env:_*)
            .run(new ProcessIO(provideIn, handleOut, handleErr))
            .exitValue()
    println("Command '''%s''' returned %s".format(cmd, code))
    stdout.close
    stderr.close
  }

  def runGetOutput(cmd: String): String = {
    def provideIn(x: OutputStream) = {
      val bash = new PrintStream(x)
      bash.println("set -eo pipefail")
      bash.println(cmd)
      bash.close()
    }
    val output = new StringBuilder
    def handleOut(x: InputStream) = { Source.fromInputStream(x).getLines().foreach( output.append(_) ) }
    def handleErr(x: InputStream) = { Source.fromInputStream(x).getLines().foreach( println(_) ) }
    var code = "bash".run(new ProcessIO(provideIn, handleOut, handleErr)).exitValue()
    println("Returned %s".format(code))
    output.toString
  }
}
