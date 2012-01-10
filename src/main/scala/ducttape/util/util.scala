package ducttape.util

import collection._
import io._
import sys.process._

import java.io._
import java.net._

object Environment {
  def getJarFile = new File(Environment.getClass.getProtectionDomain.getCodeSource.getLocation.getPath)
  def getJarDir = getJarFile.getParentFile
}

object Files {
  def write(str: String, file: File) {
    val fw = new FileWriter(file)
    fw.write(str)
    fw.close()    
  }

  def writer(file: File): PrintWriter = {
    new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8")))
  }

  def writer(opt: Option[File]): PrintWriter = opt match {
    case Some(file: File) => writer(file)
    case None => new PrintWriter(NullWriter)
  }

  // there is no reliable way of detecting symlinks in Java
  // f.getAbsolutePath != f.getCanonicalPath fails since /home/./jhclark is not canonical
  def deleteDir(dir: File) {
    val code = Shell.run("rm -rf %s".format(dir.getAbsolutePath))
    if(code != 0) {
      throw new RuntimeException("Failed to delete: %s".format(dir.getAbsolutePath))
    }
  }
}

object NullWriter extends Writer {
  override def close(): Unit = Unit
  override def flush(): Unit = Unit
  override def write(cbuf: Array[Char]): Unit = Unit
  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = Unit
}

object IO {

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
          stderrFile: File): Int = {
    run(cmds.mkString("\n"), workDir, env, stdoutFile, stderrFile)
  }

  def run(cmd: String, workDir: File, env: Seq[(String,String)],
          stdoutFile: File, stderrFile: File): Int = {
    run(cmd, Some(workDir), env, Some(stdoutFile), Some(stderrFile))
  }

  def run(cmd: String,
          workDir: Option[File] = None,
          env: Seq[(String,String)] = Seq.empty,
          stdoutFile: Option[File] = None,
          stderrFile: Option[File] = None): Int = {

    val stdout = Files.writer(stdoutFile)
    val stderr = Files.writer(stderrFile)
    def provideIn(x: OutputStream) = {
      val bash = new PrintStream(x)
      // TODO: Set environment here to be consistent with dry run script generation?
      bash.println("set -euo pipefail")
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
    stdout.close
    stderr.close
    code
  }

  def runGetOutputLinesNoShell(cmd: String,
                               workDir: File,
                               env: Seq[(String,String)],
                               stdin: Seq[String]
                               ): Seq[String] = {
    // Run command
    // TODO: How do we handle 
    def provideIn(x: OutputStream) = {
      val procin = new PrintStream(x)
      for(line <- stdin) procin.println(line)
      procin.close
    }
    var output = new mutable.ArrayBuffer[String]
    def handleOut(x: InputStream) = { Source.fromInputStream(x).getLines.foreach( output.append(_) ) }
    def handleErr(x: InputStream) = { Source.fromInputStream(x).getLines.foreach( System.err.println(_) ) }
    // pass env as varargs
    val code = Process(cmd, workDir, env:_*)
            .run(new ProcessIO(provideIn, handleOut, handleErr))
            .exitValue
    if(code != 0) {
      // TODO: More specific exception?
      throw new RuntimeException("Command '%s' returned error code %d".format(cmd, code))
    }
    output
  }

  def runGetOutputLines(cmd: String,
                        workDir: File,
                        env: Seq[(String,String)]
                       ): Seq[String] = {

    def provideIn(x: OutputStream) = {
      val bash = new PrintStream(x)
      bash.println("set -e") // stop on errors
      bash.println("set -o pipefail") // stop on errors in pipelines
      bash.println("set -u") // don't allow unbound variables
      bash.println("set -x") // show each command as it is executed
      bash.println(cmd)
      bash.close
    }
    var output = new mutable.ArrayBuffer[String]
    def handleOut(x: InputStream) = { Source.fromInputStream(x).getLines.foreach( output.append(_) ) }
    def handleErr(x: InputStream) = { Source.fromInputStream(x).getLines.foreach( System.err.println(_) ) }
    // pass env as varargs
    val code = Process("bash", workDir, env:_*)
            .run(new ProcessIO(provideIn, handleOut, handleErr))
            .exitValue
    if(code != 0) {
      // TODO: More specific exception?
      throw new RuntimeException("Command '%s' returned error code %d".format(cmd, code))
    }
    output
  }

  // TODO: Default arguments for workDir and env
  def runGetOutput(cmd: String, workDir: File, env: Seq[(String,String)]): String = {
    runGetOutputLines(cmd, workDir, env).mkString("\n")
  }
}
