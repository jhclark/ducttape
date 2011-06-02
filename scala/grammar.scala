import scala.util.parsing.combinator._
import scala.util.parsing.input._

class Task(val name: String,
           val comments: Seq[String],
           val commands: Seq[String]) {
  override def toString = name + " " + comments.toString + " " + commands
}

// see http://stackoverflow.com/questions/5063022/use-scala-parser-combinator-to-parse-csv-files
object MakelikeDSL extends RegexParsers {
  override val skipWhitespace = false

  var lineNum = 0

  //def eol = "\r\n" | "\n"
  def eol: Parser[Any] = """(\r?\n)+""".r
  //val EofCh = CharArrayReader.EofCh
  def taskBlock: Parser[Task] = comments ~ taskDef ~ commands ^^ {
    case com ~ name ~ cmds => new Task(name, com, cmds)
  }

  def comments: Parser[Seq[String]] = repsep(comment, eol) <~ (eol?)
  def comment: Parser[String] = """#\s*""".r ~> commentContent
  def commentContent: Parser[String] = """[^\r\n]+""".r

  def taskDef: Parser[String] = "[" ~> """[a-zA-Z0-9.-]+""".r <~ "]" ~ eol

  def commands: Parser[Seq[String]] = repsep(command, eol) <~ (eol?)
  def command: Parser[String] = """[^\r\n]+""".r

  def doMatch(str: String) {
    val result: ParseResult[Task] = parseAll(taskBlock, str)
    val pos: Position = result.next.pos
    result match {
      case Success(res, _) => println(res)
      case Failure(msg, _) => println("FAIL on line %d column %d: %s".format(pos.line, pos.column, msg))
      case Error(msg, _) => println("ERROR: "+msg)
    }
  }
}

MakelikeDSL.doMatch("""# hi
# how are you ?
[head-5]
cat""")

