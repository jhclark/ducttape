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


object Tests {
  
  import ducttape.syntax.AbstractSyntaxTree._
  import ducttape.syntax.Grammar._
  import ducttape.syntax.GrammarParser._
  import org.scalatest.FlatSpec
  import scala.util.parsing.combinator.Parsers
  
  def verify(testCase:FlatSpec, result:ParseResult[Any]) : Unit = {
	result match {
		case Success(res, _) => ()
		case Failure(msg, _) => testCase.fail(msg)
		case Error(msg, _)   => testCase.fail(msg)
	}
  }
  
  def verifyFailure(testCase:FlatSpec, result:ParseResult[Any]) : Unit = {
	result match {
		case Success(res, _) => testCase.fail(res.toString)
		case Failure(msg, _) => ()
		case Error(msg, _)   => testCase.fail(msg)
	}
  }
}