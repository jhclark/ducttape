// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.util

import java.security.MessageDigest

// for use with *small* strings
object HashUtils {
  def md5(s: String): String = toHex {
    // NOTE: MessageDigest is stateful, so we need a new instance every time
    val md5er = MessageDigest.getInstance("MD5")
    md5er.digest(s.getBytes)
  }
  def toHex(bytes: Array[Byte]): String = bytes.map("%02X" format(_)).mkString
}
