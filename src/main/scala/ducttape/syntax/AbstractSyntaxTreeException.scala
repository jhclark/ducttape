// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

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