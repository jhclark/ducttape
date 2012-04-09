package ducttape.syntax

import java.io.File

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharArrayReader
import scala.util.parsing.input.Position
import scala.util.parsing.input.Positional

import ducttape.syntax.AbstractSyntaxTree._
import ducttape.util.IO

object GrammarParser extends RegexParsers {

  override val skipWhitespace = false;

  private def addFileInfo(element: ASTType, file: File) {
    element.declaringFile = file
    element.children.foreach(addFileInfo(_, file))
  }
  
  def readWorkflow(file: File): WorkflowDefinition = {
    val result: ParseResult[Seq[Block]] = parseAll(Grammar.blocks, IO.read(file, "UTF-8"))    
    val pos = result.next.pos
    
    return result match {
      case Success(blocks: Seq[Block], _) => {
        blocks.foreach(addFileInfo(_, file))
        new WorkflowDefinition(file, blocks)
      }
      case Failure(msg, _) =>
        throw new FileFormatException("ERROR: line %d column %d: %s".format(pos.line, pos.column, msg), file, pos)
      case Error(msg, _) =>
        throw new FileFormatException("HARD ERROR: line %d column %d: %s".format(pos.line, pos.column, msg), file, pos)
    }    
  }
  
  def readConfig(file: File): Seq[ConfigAssignment] = {
    val result: ParseResult[Seq[ConfigAssignment]] = parseAll(Grammar.configLines, IO.read(file, "UTF-8"))    
    val pos = result.next.pos
    
    return result match {
      case Success(asses: Seq[ConfigAssignment], _) => {
       asses.foreach(addFileInfo(_, file))
       asses 
      }
      case Failure(msg, _) =>
        throw new FileFormatException("ERROR: line %d column %d: %s".format(pos.line, pos.column, msg), file, pos)
      case Error(msg, _) =>
        throw new FileFormatException("HARD ERROR: line %d column %d: %s".format(pos.line, pos.column, msg), file, pos)
    }    
  }
  
}
