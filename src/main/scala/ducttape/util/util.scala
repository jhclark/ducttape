package ducttape.util

import sys.process._
import io._

import java.io._
import java.net._

object Files {
  def write(str: String, file: File) = {
    val fw = new FileWriter(file)
    fw.write(str)
    fw.close()    
  }

  def writer(file: File) = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8")))

  def deleteDir(dir: File): Unit = {
    if(dir.isDirectory)
      for(child <- dir.listFiles)
        deleteDir(child)

    if(!dir.delete)
      throw new IOException("Could not delete file: %s".format(dir.getAbsolutePath))
  }
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

  def run(cmd: String,
          workDir: File,
          env: Seq[(String,String)],
          stdoutFile: File,
          stderrFile: File): Int = {

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
  import org.scalatest.Assertions
  import scala.util.parsing.combinator.Parsers

  /** Verify that a test case succeeded. */
  def verify(testCase:Assertions, result:ParseResult[Any]) : Unit = {
    

//    
	result match {
		case Success(res, _) => ()
		case Failure(msg, next) => testCase.fail("At position " + next.pos.toString + ": " + msg) //"At " + position+ ": "+ 
		case Error(msg, next)   => testCase.fail("At position " + next.pos.toString + ": " + msg)//("At " + position+ ": "+ msg)
	}
  }
  
  /** Verify that a test case failed in a way that the parser will not attempt to backtrack. */
  def verifyError(testCase:Assertions, result:ParseResult[Any]) : Unit = {
	result match {
		case Success(res, next) => testCase.fail("At position " + next.pos.toString + ": " + res.toString)
		case Failure(msg, next) => testCase.fail("At position " + next.pos.toString + ": Encounted Failure instead of Error: " + msg)
		case Error(msg, _)   => ()
	}
  }
  
  /** Verify that a test case failed in a way that the parser will attempt to backtrack. */
  def verifyFailure(testCase:Assertions, result:ParseResult[Any]) : Unit = {
	result match {
		case Success(res, next) => testCase.fail("At position " + next.pos.toString + ": " + res.toString)
		case Failure(msg, _) => ()
		case Error(msg, next)   => testCase.fail("At position " + next.pos.toString + ": Encounted Error instead of Failure: " + msg)
	}
  }
}
