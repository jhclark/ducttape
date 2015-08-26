// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
    // don't steamroll filenames from import statements!
    if (element.declaringFile == ASTType.UnknownFile) {
      element.declaringFile = file
    }
    element.children.foreach(addFileInfo(_, file))
  }

  def readWorkflow(file: File, isImported: Boolean = false): WorkflowDefinition = {
    val importDir: File = file.getAbsoluteFile.getParentFile
    val result: ParseResult[Seq[ASTType]] = parseAll(Grammar.elements(importDir), IO.read(file, "UTF-8"))
    val pos = result.next.pos

    return result match {
      case Success(elements: Seq[ASTType], _) => {
        elements.foreach(addFileInfo(_, file))
        new WorkflowDefinition(elements, Seq(file), isImported).collapseImports.collapseFunctionCallTasks()
      }
      case Failure(msg, _) => throw new FileFormatException(msg, file, pos)
      case Error(msg, _) => throw new FileFormatException(msg, file, pos)
    }
  }

  def readConfig(file: File): WorkflowDefinition = readWorkflow(file)
}
