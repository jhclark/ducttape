package ducttape.syntax

import AbstractSyntaxTree.ASTType

class AbstractSyntaxTreeException(element: ASTType, msg: String) 
  extends RuntimeException(
      "ERROR: line %d column %d: %s".format(
        element.endPos.line, 
        element.endPos.column, 
        msg)
      ) {

}