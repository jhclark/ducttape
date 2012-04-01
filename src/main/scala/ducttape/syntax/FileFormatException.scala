package ducttape.syntax

import java.io.File
import scala.util.parsing.input.Position

class FileFormatException(val msg: String, val refs: Seq[(File, Int, Int, Int)]) extends Exception(msg) {
  def this(msg: String, file: File, line: Int, col: Int) = this(msg, List( (file, line, col, line) ))
  def this(msg: String, file: File, pos: Position) = this(msg, List( (file, pos.line, pos.column, pos.line) ))
  // require list instead of Seq to get around erasure
  def this(msg: String, refs: Iterable[(File, Position)]) = this(msg, (for( (f,p) <- refs) yield (f, p.line, p.column, p.line)).toList )
  def this(msg: String, refs: List[(File, Position, Int)]) = this(msg, for( (f,p,until) <- refs) yield (f, p.line, p.column, until) )
}
