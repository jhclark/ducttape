// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.syntax

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional
import java.io.File
import ducttape.syntax.AbstractSyntaxTree.ASTType
import ducttape.syntax.AbstractSyntaxTree.BashCode
// TODO: Move this into unit tests
/*
object BashParser extends App with RegexParsers {


  override val skipWhitespace = false

  // TODO: Throw warnings on constructs that are known not to return error codes properly?
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
echo \\\"\'hi
escaping='\'\\'
escaping="\'\"\$\\"
y="another confusing string{"
string_with_var="$doubleQuoted $$ '$not_a_variable'"
function ohai {
  $a0
}
$moses < $in > $out
pid=$$
x=\$not_a_variable
subproc=$(echo $(echo $hi))
string_len=${#pid}
string_manip=${$pid:0:5}
cat <<EOF
{ ${with_a_variable}
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
*/





/**
 * Very simple grammar for bash
 *
 * @author Jon Clark
 */
object BashGrammar {

  import ducttape.syntax.GrammarParser._ // we need visibility of Parser, etc.

  // "... there are dark corners in the Bourne shell, and people use all of them."
  // --Chet Ramey

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

  def singleQuoteStringLiteral: Parser[BashCode] = (literal("'") ~ singleQuoteContent ~ literal("'")) ^^ {
    case open ~ str ~ close => new BashCode(open + str + close)
  }

  def singleQuoteContent: Parser[String] = (regex("""[^'\\]*""".r) ~ opt(escapedChar ~ singleQuoteContent)) ^^ {
    case content ~ None => content
    case content ~ Some(escaped ~ content2) => content + escaped + content2
  }

  def doubleQuoteStringLiteral: Parser[BashCode] = (literal("\"") ~ doubleQuoteContent ~ literal("\"")) ^^ {
    case open ~ dq ~ close => new BashCode(open + dq + close, dq.vars)
  }

  def doubleQuoteContent: Parser[BashCode] = regex("[^$\"\\\\]*".r) ~ opt(doubleQuoteSpecialContent ~ doubleQuoteContent) ^^ {
    case before ~ None => new BashCode(before)
    case before ~ Some(special ~ after) => new BashCode(before + special + after, special.vars ++ after.vars)
  }

  def doubleQuoteSpecialContent: Parser[BashCode] = variableLike | singleQuoteStringLiteral | escapedChar

  def escapedChar: Parser[BashCode] = (literal("\\") ~ regex(".".r)) ^^ {
    case escaper ~ escaped => new BashCode(escaper + escaped)
  }

  def escapedNewline: Parser[BashCode] = literal("\\\n") ^^ {
    case _ => new BashCode("\\\n")
  }

  def stringLiteral: Parser[BashCode] = singleQuoteStringLiteral | doubleQuoteStringLiteral

  // never match any paired elements
  // nor any escape sequences
  // and special case << for heredocs and process substitutions
  def codeBlob: Parser[String] = (regex("[^{}()\"'#$<\\\\]*".r) ~ opt(regex("<[^<(]".r) ~ codeBlob)) ^^ {
    case blob ~ None => blob
    case blob1 ~ Some(re ~ blob2) => blob1 + re + blob2
  }

  def curlySection: Parser[BashCode] = (literal("{") ~ bashBlock ~ literal("}")) ^^ {
    case open ~ b ~ close => new BashCode(open + b + close, b.vars)
  }

  def parenSection: Parser[BashCode] = (literal("(") ~ bashBlock ~ literal(")")) ^^ {
    case open ~ b ~ close => new BashCode(open + b + close, b.vars)
  }

  // named variables $hi and ${hi}, builtin variable $$ or $? or $!, command substitution $(echo), or string expansion $'\n', etc.
  // NOTE: String manipulation must be matched *after* variable
  // See http://tldp.org/LDP/abs/html/bashver2.html for more on string expansion
  def variableLike: Parser[BashCode] = dollarOnly | internalVariable | commandSub | variable | stringManipulation | stringExpansion

  // command substitution: $(echo)
  def commandSub: Parser[BashCode] = (literal("$(") ~ bashBlock ~ literal(")")) ^^ {
    case open ~ b ~ close => new BashCode(open + b + close, b.vars)
  }

  // process substitution: <(zcat x.gz)
  def inProcessSub: Parser[BashCode] = (literal("<(") ~ bashBlock ~ literal(")")) ^^ {
    case open ~ b ~ close => new BashCode(open + b + close, b.vars)
  }

  // process substitution: <(zcat x.gz)
  def outProcessSub: Parser[BashCode] = (literal(">(") ~ bashBlock ~ literal(")")) ^^ {
    case open ~ b ~ close => new BashCode(open + b + close, b.vars)
  }

  // allow "echo $" or "echo $ cat" or "echo $;" to mean a literal dollar
  def dollarOnly: Parser[BashCode] = regex("""\$[ \t\r\n]+""".r) ^^ {
    case x => new BashCode(x)
  }

  // TODO: Warn on process substitutions since they don't error out properly

  /**
   * Bash positional and special parameters.
   *
   * @see The GNU Bash Reference Manual, section 3.4.1 "Positional Parameters"
   * @see The GNU Bash Reference Manual, section 3.4.2 "Special Parameters"
   */
  def internalVariable: Parser[BashCode] = (
      literal("$*") |
      literal("$@") |
      literal("$#") |
      literal("$?") |
      literal("$-") |
      literal("$$") |
      literal("$!") |
      literal("$0") |
      literal("$_") |
      regex("""\$[1-9][0-9]*""".r)
    ) ^^ {
    case x => new BashCode(x)
  }

  /* see http://tldp.org/LDP/abs/html/string-manipulation.html
   * string manipulations can contain arbitrary regular expressions
   * we don't match any variables inside this mess */
  def stringManipulation: Parser[BashCode] = (literal("${") ~ regex("[^}]+".r) ~ literal("}")) ^^ {
    case open ~ content ~ close => new BashCode(open + content + close)
  }

  def variable: Parser[BashCode] = {
    (literal("$") ~ variableName) |
    (literal("$") ~ literal("{") ~ variableName ~ literal("}")) |
    (literal("$") ~ literal("{") ~ variableName ~ failure("Missing closing } in bash variable reference"))
  } ^^ {
    case (dollar:String) ~ (name:String) => new BashCode(dollar + name, Set(name))
    case (dollar:String) ~ (open:String) ~ (name:String) ~ (close:String) => new BashCode(dollar + open + name + close, Set(name))
  }

  def stringExpansion: Parser[BashCode] = regex("""\$'\\[A-Za-z0-9]+'""".r) ^^ {
    case (str:String) => new BashCode(str)
  }

  def variableName: Parser[String] = regex("[A-Za-z_][A-Za-z0-9_]*".r)

  def heredoc: Parser[BashCode] = (literal("<<") ~ opt(literal("-")) ~ heredocMarker ~ eol ~ heredocContent ~ literal("EOF") ~ eol) ^^ {
    case double ~ None ~ marker1 ~ e1 ~ hdoc ~ eof ~ e2  => new BashCode(double + marker1 + e1 + hdoc + eof + e2, hdoc.vars)
    case double ~ dash ~ marker1 ~ e1 ~ hdoc ~ eof ~ e2 => new BashCode(double + dash + marker1 + e1 + hdoc + eof + e2, hdoc.vars)
  }

  // TODO: Throw an error if user tries to use anything other than EOF
  def heredocMarker: Parser[String] = literal("EOF")

  // TODO: Recognize variables inside heredocs
  def heredocLine: Parser[BashCode] = not(literal("EOF")) ~> regex("""[^\r\n]*\n""".r) ^^ {
    case line => new BashCode(line)
  }

  def heredocContent: Parser[BashCode] = rep(heredocLine) ^^ {
    case list => new BashCode(list.map(_.toString).mkString(""), list.flatMap(_.vars).toSet)
  }

  def nonBlobElement: Parser[BashCode] = escapedChar | escapedNewline | variableLike |
    inProcessSub | outProcessSub | parenSection | curlySection | stringLiteral | comment | heredoc

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
