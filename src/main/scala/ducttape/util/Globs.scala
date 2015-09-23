// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.util

object Globs {
  // note: we don't handle the "strict wildcard" where * can't match directory slashes
  def globToRegex(glob: String): String = {
    var curlies = 0
    glob.map {
      _ match {
        case ',' if (curlies > 0) => '|'
        case '}' if (curlies > 0) => {
          curlies -= 1
          ")"
        }
        case '{' => {
          curlies += 1
          "("
        }
        
        case '*' => ".*"
        case '?' => "."
        case esc @ ('.' | '\\' | '+' | '(' | ')' | '|' | '^' | '$' | '@' | '%') => "\\" + esc
        case char @ _ => char
      }
    }.mkString("")
  }
}

