//package ducttape.io
//
//import scala.io._
//import scala.util.parsing.combinator._
//import scala.util.parsing.input._
//
//import java.io.File
//
//import ducttape.Types._
//
//class FileFormatException(msg: String) extends Exception(msg) {}
//
//class WorkflowDefinition(val tasks: Seq[TaskDef]) {}
//class TaskDef(val name: String,
//              val comments: Seq[String],
//              val inputs: Seq[Spec],
//              val outputs: Seq[Spec],
//              val params: Seq[Spec],
//              val commands: Seq[String]) {
//  override def toString = name + " " + comments.toString + " " + inputs + " " + outputs + " " + commands
//}
//class TaskHeader(val name: String, val inputs: Seq[Spec], val outputs: Seq[Spec], val params: Seq[Spec]) {}
//class SpecT[+RValueT](val name: String, val rval: RValueT /*, val file: File, val line: Int*/) {
//  //def fileline = file.getAbsolutePath + ":" + line
//  override def toString = name + "=" + rval
//}
//
//abstract class RValue;
//case class Unbound extends RValue {
//  override def toString = ""
//}
//case class Literal(value: String) extends RValue {
//  override def toString = value
//}
//case class Variable(task: String, value: String) extends RValue {
//  override def toString = "$" + task + "/" + value
//}
//
//// see http://stackoverflow.com/questions/5063022/use-scala-parser-combinator-to-parse-csv-files
//object MakelikeDSL extends RegexParsers {
//  override val skipWhitespace = false
//
//  def eol = "\r\n" | "\n" | CharArrayReader.EofCh
//  def space = """[ \t]+""".r
//  // actually consumes previous EOL and leaves next for use with repsep
//  def emptyline = ("\r\n" | "\n") ~ """[ \t]*""".r ~ guard(eol)
//  def workflow: Parser[WorkflowDefinition] = tasks ^^ { // TODO: emptylines
//    case w => new WorkflowDefinition(w)
//  }
//  def tasks: Parser[Seq[TaskDef]] = repsep(taskBlock, eol)
//  def taskBlock: Parser[TaskDef] = comments ~ taskHeader ~ commands <~ (emptyline*) ^^ {
//    case com ~ head ~ cmds => new TaskDef(head.name, com, head.inputs, head.outputs, head.params, cmds)
//  }
//  def comments: Parser[Seq[String]] = repsep(comment, eol) <~ (eol?)
//  def comment: Parser[String] =  """#[ \t]*""".r ~> commentContent <~ (emptyline*)
//  def commentContent: Parser[String] = """[^\r\n]*""".r
//
//  // deps CANNOT be separated by a newline or else we don't know where commands begin
//  def taskHeader: Parser[TaskHeader]
//    = taskName ~ (space*) ~ taskInputs ~ (space*) ~ taskOutputs ~ (space*) ~ taskParams <~ eol ^^ {
//      case name ~ sp1 ~ targets ~ sp2 ~ deps ~ sp3 ~ params => new TaskHeader(name, targets, deps, params)
//    }
//  def taskName: Parser[String] = "[" ~> name <~ "]"
//  def taskInputs: Parser[Seq[Spec]] = opt("<" ~ rep(space) ~> repsep(assignment, space)) ^^ {
//    case Some(list) => list
//    case None => List.empty
//  }
//  // TODO: Disallow variables here
//  def taskOutputs: Parser[Seq[Spec]] = opt(">" ~ rep(space) ~> repsep(assignment, space)) ^^ {
//    case Some(list) => list
//    case None => List.empty
//  }
//  def taskParams: Parser[Seq[Spec]] = opt("::" ~ rep(space) ~> repsep(assignment, space)) ^^ {
//    case Some(params) => params
//    case None => List.empty
//  }
//  // TODO: Inputs without = (None assignments)
//  def assignment: Parser[Spec] = name ~ opt("=" ~ rvalue) ^^ {
//    case strVar ~ Some(e ~ value) => new Spec(strVar, value)
//    case strVar ~ None => new Spec(strVar, Unbound())
//  }
//  // TODO: Rewrite using |
//  def rvalue: Parser[RValue] = opt("$" ~ name ~ "/") ~ value ^^ {
//    case None ~ strVal => new Literal(strVal)
//    case Some("$" ~ strTask ~ slash) ~ strVal => new Variable(strTask, strVal)
//  }
//
//  def commands: Parser[Seq[String]] = repsep(command, eol) <~ (eol?)
//  def command: Parser[String] = rep1(space) ~> """[^\r\n]+""".r
//
//  // bash doesn't allow . in environment variables
//  def name: Parser[String] = """[A-Za-z0-9_-]+""".r // [^\[\]\r\n:. \t=<>]
//  def value: Parser[String] = """[^\r\n: \t]+""".r
//
//  // TODO: Show what element (or element tree?) we were expecting when we failed
//  def read(file: File): WorkflowDefinition = {
//    val result: ParseResult[WorkflowDefinition] = parseAll(workflow, Source.fromFile(file, "UTF-8").reader)
//    val pos = result.next.pos
//    result match {
//      case Success(res, _) => res
//      case Failure(msg, _) =>
//       throw new FileFormatException("ERROR: line %d column %d: %s".format(pos.line, pos.column, msg))
//      case Error(msg, _) =>
//       throw new FileFormatException("HARD ERROR: line %d column %d: %s".format(pos.line, pos.column, msg))
//    }
//  }
//}
