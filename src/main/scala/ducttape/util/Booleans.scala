// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.util

object Booleans {
  def parseBoolean(str: String): Boolean = str.toLowerCase match {
    case "true" => true
    case "false" => false

    case "1" => true
    case "0" => false

    case "enable" => true
    case "disable" => false
    
    case "t" => true
    case "f" => false
  }
}
