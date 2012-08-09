package ducttape.util

import java.security.MessageDigest

// for use with *small* strings
object HashUtils {
  private val md5er = MessageDigest.getInstance("MD5")
  def md5(s: String): String = toHex(md5er.digest(s.getBytes))
  def toHex(bytes: Array[Byte]): String = bytes.map("%02X" format(_)).mkString
}
