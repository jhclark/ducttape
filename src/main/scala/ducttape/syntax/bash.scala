package ducttape.syntax 

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

import java.io.File

object BashParser extends App with RegexParsers {
  	

  override val skipWhitespace = false

  println("heredoc:")
  val heredocResult: ParseResult[BashCode] = parseAll(BashGrammar.heredoc, """<<EOF
{
EOF
""")
  println(heredocResult)


  // use file over reader to provide more informative error messages
  // file must be UTF-8 encoded
  println("Should succeed:")
  val goodResult: ParseResult[BashCode] = parseAll(BashGrammar.bashBlock, """
#!/usr/bin/env bash
x=$y
( # stuff in parens launches subprocess
  x=${y}
  echo 'hi' # let's confuse the parser: {
  z='confusing string {'
)
y="another confusing string{"
string_with_var="$doubleQuoted"
function ohai {
  $a0
}
$moses < $in > $out
pid=$$
subproc=$(echo $(echo $hi))
string_len=${#pid}
string_manip=${$pid:0:5}
cat <<EOF
{
EOF
"""
)
  println(goodResult)
  println("Variable list: " + goodResult.get.vars)


println("Let's fail:")
  val badResult: ParseResult[BashCode] = parseAll(BashGrammar.bashBlock, """
#!/usr/bin/env bash
x=$y
echo 'hi' # let's confuse it: {
function ohai {
  $a0
"""
)
  println(badResult)

}

// TODO: Pass a StringBuilder down through the AST to make stringification faster
class BashCode(val code: String, val vars: Set[String] = Set.empty) {
  override def toString = code
}

/**
 * Very simple grammar for bash
 * 
 * @author Jon Clark
 */
object BashGrammar {

  // "... there are dark corners in the Bourne shell, and people use all of them."
  // --Chet Ramey

  import BashParser._

  // === WHITE SPACE ===

  /** End of line characters, including end of file. */
  val eol: Parser[String] = literal("\r\n") | literal("\n") | regex("""\z""".r) | literal(CharArrayReader.EofCh.toString)

  /** Non-end of line white space characters */
  val space: Parser[String] = regex("""[ \t]+""".r)

  /** A single line of comment. It is paired in that the # and \n bound the comment */
  val comment: Parser[BashCode] = (regex("""[ \n\r\t]*#[ \t]*""".r) ~ commentContent ~ eol) ^^ {
    case re ~ content ~ e => new BashCode(re + content + e)
  }

  /** The content portion of a single line of comment. 
   *  Notably, this excludes the syntactic comment marker itself. 
   */
  val commentContent: Parser[String] = """[^\r\n]*""".r

  def singleQuoteStringLiteral: Parser[BashCode] = (literal("'") ~ regex("""[^']*""".r) ~ literal("'")) ^^ {
    case open ~ str ~ close => new BashCode(open + str + close)
  }

  def doubleQuoteStringLiteral: Parser[BashCode] = (literal("\"") ~ doubleQuoteContent ~ literal("\"")) ^^ {
    case open ~ dq ~ close => new BashCode(open + dq + close, dq.vars)
  }

  def doubleQuoteContent: Parser[BashCode] = regex("[^$\"]*".r) ~ opt(doubleQuoteSpecialContent ~ doubleQuoteContent) ^^ {
    case before ~ None => new BashCode(before)
    case before ~ Some(special ~ after) => new BashCode(before + special + after, special.vars ++ after.vars)
  }
  
  def doubleQuoteSpecialContent: Parser[BashCode] = variableLike | stringLiteral

  def stringLiteral: Parser[BashCode] = singleQuoteStringLiteral | doubleQuoteStringLiteral

  // never match any paired elements
  // and special case << for heredocs
  def codeBlob: Parser[String] = (regex("[^{}()\"'#$<]*".r) ~ opt(regex("<[^<]".r) ~ codeBlob)) ^^ {
    case blob ~ None => blob
    case blob1 ~ Some(re ~ blob2) => blob1 + re + blob2
  }

  def curlySection: Parser[BashCode] = (literal("{") ~ bashBlock ~ literal("}")) ^^ {
    case open ~ b ~ close => new BashCode(open + b + close, b.vars)
  }

  def parenSection: Parser[BashCode] = (literal("(") ~ bashBlock ~ literal(")")) ^^ {
    case open ~ b ~ close => new BashCode(open + b + close, b.vars)
  }

  // named variables $hi and ${hi}, builtin variable $$ or $? or $!, command substitution $(echo), etc.
  // NOTE: String manipulation must be matched *after* variable
  def variableLike: Parser[BashCode] = internalVariable | commandSub | variable | stringManipulation

  // command substitution: $(echo)
  def commandSub: Parser[BashCode] = (literal("$(") ~ bashBlock ~ literal(")")) ^^ {
    case open ~ b ~ close => new BashCode(open + b + close, b.vars)
  }

  /* see http://tldp.org/LDP/abs/html/internalvariables.html
   * we intentially don't support script parameters: $@ $* $- $# $0 $1, etc. */
  def internalVariable: Parser[BashCode] = (literal("$$") | literal("$?") | literal("$!") | literal("$_")) ^^ {
    case x => new BashCode(x)
  }

  /* see http://tldp.org/LDP/abs/html/string-manipulation.html
   * string manipulations can contain arbitrary regular expressions
   * we don't match any variables inside this mess */
  def stringManipulation: Parser[BashCode] = (literal("${") ~ regex("[^}]+".r) ~ literal("}")) ^^ {
    case open ~ content ~ close => new BashCode(open + content + close)
  }

  def variable: Parser[BashCode] = (literal("$") ~ opt(literal("{")) ~ variableName ~ opt(literal("}"))) ^^ {
    case dollar ~ None ~ name ~ None => new BashCode(dollar + name, Set(name))
    case dollar ~ Some(open) ~ name ~ Some(close) => new BashCode(dollar + open + name + close, Set(name))
  }

  def variableName: Parser[String] = regex("[A-Za-z_][A-Za-z0-9_]*".r)

  def heredoc: Parser[BashCode] = (literal("<<") ~ opt(literal("-")) ~ heredocMarker ~ eol ~ heredocContent ~ literal("EOF") ~ eol) ^^ {
    case double ~ None ~ marker1 ~ e1 ~ hdoc ~ eof ~ e2  => new BashCode(double + marker1 + e1 + hdoc + eof + e2)
    case double ~ dash ~ marker1 ~ e1 ~ hdoc ~ eof ~ e2 => new BashCode(double + dash + marker1 + e1 + hdoc + eof + e2)
  }

  // TODO: Throw an error if user tries to use anything other than EOF
  def heredocMarker: Parser[String] = literal("EOF")

  def heredocLine: Parser[String] = not(literal("EOF")) ~> regex("""[^\r\n]*\n""".r)

/*
  def heredocContent: Parser[String] = rep(heredocLine) ^^ {
    case list => list.mkString("")
  }
*/
  def heredocContent: Parser[String] = literal("{\n")

  def nonBlobElement: Parser[BashCode] = variableLike | parenSection | curlySection | stringLiteral | comment | heredoc

// Done: Strings
// Done: Comments
// Done: Variables
// Done: Heredocs (Restricted to a single non-arbitrary EOF marker)
// Done: Process substitutions
// Done: Variable mangling and string operations
// TODO: Vars in double quote strings
// Done: Pass up list of variables so that we can see if they're declared or unused (Warn if not)
  def bashBlock: Parser[BashCode] = {
     codeBlob ~ opt(nonBlobElement ~ bashBlock) ^^ {
      case before ~ None => new BashCode(before)
      case before ~ Some(nonblob ~ after) => new BashCode(before + nonblob + after, nonblob.vars ++ after.vars)
    }
  }
}
