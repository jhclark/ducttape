package ducttape.util

import collection._
import io._
import sys.process._

import java.io._
import java.net._

import grizzled.slf4j.Logging

object Shell extends Logging {
  def run(cmds: Seq[String],
          stdPrefix: String,
          workDir: File,
          env: Seq[(String,String)],
          stdoutFile: File,
          stderrFile: File): Int = {
    run(cmds.mkString("\n"), stdPrefix, workDir, env, stdoutFile, stderrFile)
  }

  def run(cmd: String,
          stdPrefix: String,
          workDir: File,
          env: Seq[(String,String)],
          stdoutFile: File,
          stderrFile: File): Int = {
    run(cmd, stdPrefix, Some(workDir), env, Some(stdoutFile), Some(stderrFile))
  }

  def run(cmd: String,
          stdPrefix: String,
          workDir: Option[File] = None,
          env: Seq[(String,String)] = Seq.empty,
          stdoutFile: Option[File] = None,
          stderrFile: Option[File] = None): Int = {

    val stdout = Files.writer(stdoutFile)
    val stderr = Files.writer(stderrFile)
    def provideIn(x: OutputStream) = {
      val bash = new PrintStream(x)
      // TODO: Set environment here to be consistent with dry run script generation?
      bash.println(BASH_FLAGS.mkString("\n"))
      bash.println(cmd)
      bash.close()
    }
    def handleOut(x: InputStream) = for (line <- Source.fromInputStream(x).getLines) {
      // avoid concatenation
      System.out.print(stdPrefix)
      System.out.print(": ")
      System.out.println(line)
      stdout.print(line) // no flush like println
      stdout.append('\n')
    }
    def handleErr(x: InputStream) = for (line <- Source.fromInputStream(x).getLines) {
      // avoid concatenation
      System.err.print(stdPrefix)
      System.err.print(": ")
      System.err.println(line)
      System.err.flush()
      stderr.print(line)
      stderr.append('\n')
    }
    // pass env as varargs
    val code = Process("bash", workDir, env:_*)
            .run(new ProcessIO(provideIn, handleOut, handleErr))
            .exitValue
    stdout.close()
    stderr.close()
    code
  }

  def runGetOutputLinesNoShell(cmd: String,
                               stdPrefix: String,
                               workDir: File,
                               env: Seq[(String,String)],
                               stdin: Seq[String]
                               ): Seq[String] = {
    // Run command
    // TODO: How do we handle 
    def provideIn(x: OutputStream) = {
      val procin = new PrintStream(x)
      for (line <- stdin) procin.println(line)
      procin.close
    }
    val output = new mutable.ArrayBuffer[String]
    def handleOut(x: InputStream) = Source.fromInputStream(x).getLines.foreach { output.append(_) }
    def handleErr(x: InputStream) = Source.fromInputStream(x).getLines.foreach { line =>
      // avoid concatenation
      System.out.print(stdPrefix)
      System.out.print(": ")
      System.out.println(line)
    }
    // pass env as varargs
    val code = Process(cmd, workDir, env:_*)
            .run(new ProcessIO(provideIn, handleOut, handleErr))
            .exitValue
    if (code != 0) {
      // TODO: More specific exception?
      throw new RuntimeException("Command '%s' returned error code %d".format(cmd, code))
    }
    output
  }

  val BASH_FLAGS = Seq(
    "set -e", // stop on errors
    "set -o pipefail", // stop on errors in pipelines
    "set -u", // don't allow unbound variables
    "set -x" // show each command as it is executed
  )

  def runGetOutputLines(cmd: String,
                        stdPrefix: String,
                        workDir: File,
                        env: Seq[(String,String)]
                       ): Seq[String] = {

    def provideIn(x: OutputStream) = {
      val bash = new PrintStream(x)
      bash.println(BASH_FLAGS.mkString("\n"))
      bash.println(cmd)
      bash.close
    }
    val output = new mutable.ArrayBuffer[String]
    def handleOut(x: InputStream) = Source.fromInputStream(x).getLines.foreach { output.append(_) }
    def handleErr(x: InputStream) = Source.fromInputStream(x).getLines.foreach { line =>
      System.err.print(stdPrefix)
      System.err.print(": ")
      System.err.println(line)
    }
    // pass env as varargs
    val code = Process("bash", workDir, env:_*).
                run(new ProcessIO(provideIn, handleOut, handleErr)).
                exitValue
    if (code != 0) {
      // TODO: More specific exception?
      throw new RuntimeException("Command '%s' returned error code %d".format(cmd, code))
    }
    output
  }

  // TODO: Default arguments for workDir and env
  def runGetOutput(cmd: String, stdPrefix: String, workDir: File, env: Seq[(String,String)]): String = {
    runGetOutputLines(cmd, stdPrefix, workDir, env).mkString("\n")
  }
}
