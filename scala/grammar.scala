package ducttape.io

import scala.io._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

class FileFormatException(msg: String) extends Exception(msg) {}

class TaskDef(val name: String, val targets: Seq[String], val deps: Seq[DependencySpec], val params: Seq[ParamSpec]) {}
class DependencySpec(val name: String, val value: Option[String]) {}
class ParamSpec(val name: String, val value: Option[String]) {}

class Task(val name: String,
           val comments: Seq[String],
           val inputs: Seq[DependencySpec],
           val outputs: Seq[String],
           val params: Seq[ParamSpec],
           val commands: Seq[String]) {
  override def toString = name + " " + comments.toString + " " + inputs + " " + outputs + " " + commands
}

// see http://stackoverflow.com/questions/5063022/use-scala-parser-combinator-to-parse-csv-files
object MakelikeDSL extends RegexParsers {
  override val skipWhitespace = false

  var lineNum = 0

  def eol = "\r\n" | "\n" | CharArrayReader.EofCh
  def space = """[ \t]+""".r
  def tasks: Parser[Seq[Task]] = repsep(taskBlock, eol)
  def taskBlock: Parser[Task] = comments ~ taskDef ~ commands ^^ {
    case com ~ td ~ cmds => new Task(td.name, com, td.deps, td.targets, td.params, cmds)
  }

  def comments: Parser[Seq[String]] = repsep(comment, eol) <~ (eol?)
  def comment: Parser[String] = """#\s*""".r ~> commentContent
  def commentContent: Parser[String] = """[^\r\n]+""".r

  // deps CANNOT be separated by a newline or else we don't know where commands begin
  def taskDef: Parser[TaskDef]
    = taskName ~ rep(space) ~ taskTargets ~ rep(space) ~ taskDeps ~ rep(space) ~ taskParams <~ eol ^^ {
      case name ~ sp1 ~ targets ~ sp2 ~ deps ~ sp3 ~ params => new TaskDef(name, targets, deps, params)
    }
  def taskName: Parser[String] = "[" ~> """[a-zA-Z0-9.-]+""".r <~ "]"
  def taskTargets: Parser[Seq[String]] = repsep(taskTarget, space)
  def taskTarget: Parser[String] = variable
  def taskDeps: Parser[Seq[DependencySpec]] = opt(">" ~ rep(space) ~> repsep(taskDep, space)) ^^ {
    case Some(deps) => deps
    case None => List.empty
  }
  // TODO: Inputs without =
  def taskDep: Parser[DependencySpec] = variable ~ "=" ~ value ^^ {
    case strVar ~ x ~ strVal => new DependencySpec(strVar, Some(strVal))
  }
  def taskParams: Parser[Seq[ParamSpec]] = opt(":" ~ rep(space) ~> repsep(taskParam, space)) ^^ {
    case Some(params) => params
    case None => List.empty
  }
  // TODO: Params without =
  def taskParam: Parser[ParamSpec] = variable ~ "=" ~ value ^^ {
    case strVar ~ x ~ strVal => new ParamSpec(strVar, Some(strVal))
  }

  def commands: Parser[Seq[String]] = repsep(command, eol) <~ (eol?)
  def command: Parser[String] = rep1(space) ~> """[^\r\n]+""".r

  def variable: Parser[String] = """[^\r\n: \t=]+""".r
  def value: Parser[String] = """[^\r\n: \t]+""".r

  def read(file: String): Seq[Task] = {
    val result: ParseResult[Seq[Task]] = parseAll(tasks, Source.fromFile(file, "UTF-8").reader)
    val pos = result.next.pos
    result match {
      case Success(res, _) => res
      case Failure(msg, _) =>
       throw new FileFormatException("ERROR: line %d column %d: %s".format(pos.line, pos.column, msg))
      case Error(msg, _) =>
       throw new FileFormatException("HARD ERROR: line %d column %d: %s".format(pos.line, pos.column, msg))
    }
  }
}

