import scala.util.parsing.combinator._
import scala.util.parsing.input._

class Task(val name: String,
           val comments: Seq[String],
           val inputs: Seq[String],
           val outputs: Seq[String],
           val commands: Seq[String]) {
  override def toString = name + " " + comments.toString + " " + inputs + " " + outputs + " " + commands
}

// see http://stackoverflow.com/questions/5063022/use-scala-parser-combinator-to-parse-csv-files
object MakelikeDSL extends RegexParsers {
  override val skipWhitespace = false

  var lineNum = 0

  class TaskDef(val name: String,
                val targets: Seq[String],
                val deps: Seq[String]) {}

  def eol = "\r\n" | "\n" | CharArrayReader.EofCh
  def space = """[ \t]+""".r
  def taskBlock: Parser[Task] = comments ~ taskDef ~ commands ^^ {
    case com ~ td ~ cmds => new Task(td.name, com, td.deps, td.targets, cmds)
  }

  def comments: Parser[Seq[String]] = repsep(comment, eol) <~ (eol?)
  def comment: Parser[String] = """#\s*""".r ~> commentContent
  def commentContent: Parser[String] = """[^\r\n]+""".r

  // deps CANNOT be separated by a newline or else we don't know where commands begin
  def taskDef: Parser[TaskDef] = taskName ~ taskTargets ~ taskDeps <~ eol ^^ {
    case name ~ targets ~ deps => new TaskDef(name, targets, deps)
  }
  def taskName: Parser[String] = "[" ~> """[a-zA-Z0-9.-]+""".r <~ "]"
  def taskTargets: Parser[Seq[String]] = repsep(taskTarget, space) 
  def taskTarget: Parser[String] = """[^\r\n:]+""".r
  def taskDeps: Parser[Seq[String]] = opt("""\s*:\s*""".r ~> repsep(taskDep, space)) ^^ {
    case Some(deps) => deps
    case None => List.empty
  }
  def taskDep: Parser[String] = """[^\r\n:]+""".r

  def commands: Parser[Seq[String]] = repsep(command, eol) <~ (eol?)
  def command: Parser[String] = """[^\r\n]+""".r

  def doMatch(str: String) {
    val result: ParseResult[Task] = parseAll(taskBlock, str)
    val pos: Position = result.next.pos
    result match {
      case Success(res, _) => println(res)
      case Failure(msg, _)
        => println("FAIL on line %d column %d: %s".format(pos.line, pos.column, msg))
      case Error(msg, _) => println("ERROR: "+msg)
    }
  }
}

MakelikeDSL.doMatch("""# hi
# how are you ?
[head-5] a b c : x y z
cat""")

