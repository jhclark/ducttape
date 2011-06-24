package ducttape.io

import scala.io._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

class FileFormatException(msg: String) extends Exception(msg) {}

class WorkflowDefinition(val tasks: Seq[TaskDef]) {}
class TaskDef(val name: String,
              val comments: Seq[String],
              val inputs: Seq[Spec],
              val outputs: Seq[String],
              val params: Seq[Spec],
              val commands: Seq[String]) {
  override def toString = name + " " + comments.toString + " " + inputs + " " + outputs + " " + commands
}
class TaskHeader(val name: String, val targets: Seq[String], val deps: Seq[Spec], val params: Seq[Spec]) {}
class Spec(val name: String, val value: RValue) {}

abstract class RValue;
case class Unbound extends RValue;
case class Literal(value: String) extends RValue;
case class Variable(task: String, value: String) extends RValue;

// see http://stackoverflow.com/questions/5063022/use-scala-parser-combinator-to-parse-csv-files
object MakelikeDSL extends RegexParsers {
  override val skipWhitespace = false

  var lineNum = 0

  def eol = "\r\n" | "\n" | CharArrayReader.EofCh
  def space = """[ \t]+""".r

  def workflow: Parser[WorkflowDefinition] = tasks ^^ { w => new WorkflowDefinition(w) }
  def tasks: Parser[Seq[TaskDef]] = repsep(taskBlock, eol)
  def taskBlock: Parser[TaskDef] = comments ~ taskHeader ~ commands ^^ {
    case com ~ td ~ cmds => new TaskDef(td.name, com, td.deps, td.targets, td.params, cmds)
  }

  def comments: Parser[Seq[String]] = repsep(comment, eol) <~ (eol?)
  def comment: Parser[String] = """#\s*""".r ~> commentContent
  def commentContent: Parser[String] = """[^\r\n]+""".r

  // deps CANNOT be separated by a newline or else we don't know where commands begin
  def taskHeader: Parser[TaskHeader]
    = taskName ~ rep(space) ~ taskTargets ~ rep(space) ~ taskDeps ~ rep(space) ~ taskParams <~ eol ^^ {
      case name ~ sp1 ~ targets ~ sp2 ~ deps ~ sp3 ~ params => new TaskHeader(name, targets, deps, params)
    }
  def taskName: Parser[String] = "[" ~> name <~ "]"
  def taskTargets: Parser[Seq[String]] = repsep(taskTarget, space)
  def taskTarget: Parser[String] = name
  def taskDeps: Parser[Seq[Spec]] = opt(">" ~ rep(space) ~> repsep(assignment, space)) ^^ {
    case Some(deps) => deps
    case None => List.empty
  }
  def taskParams: Parser[Seq[Spec]] = opt(":" ~ rep(space) ~> repsep(assignment, space)) ^^ {
    case Some(params) => params
    case None => List.empty
  }
  // TODO: Inputs without = (None assignments)
  def assignment: Parser[Spec] = name ~ "=" ~ rvalue ^^ {
    case strVar ~ e ~ value => new Spec(strVar, value)
  }
  // TODO: Rewrite using |
  def rvalue: Parser[RValue] = opt("$" ~ name ~ "/") ~ value ^^ {
    case None ~ strVal => new Literal(strVal)
    case Some("$" ~ strTask ~ slash) ~ strVal => new Variable(strTask, strVal)
  }

  def commands: Parser[Seq[String]] = repsep(command, eol) <~ (eol?)
  def command: Parser[String] = rep1(space) ~> """[^\r\n]+""".r

  def name: Parser[String] = """[^\[\]\r\n: \t=]+""".r
  def value: Parser[String] = """[^\r\n: \t]+""".r

  def read(file: String): WorkflowDefinition = {
    val result: ParseResult[WorkflowDefinition] = parseAll(workflow, Source.fromFile(file, "UTF-8").reader)
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
