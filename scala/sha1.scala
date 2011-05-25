import java.security.MessageDigest

object FileUtils {
  import java.io.File
  import java.io.FileInputStream
  import java.nio.channels.FileChannel.MapMode._
  
  private val SIZE = 1024;

  def read(filename: String, f: Array[Byte] => Unit) = {
    val file = new File(filename)
    val fileSize = file.length
    val stream = new FileInputStream(file)
    val buf = stream.getChannel.map(READ_ONLY, 0, fileSize)
    var arr = new Array[Byte](SIZE);
    while(buf.hasRemaining) {
      if(buf.remaining < arr.length)
        arr = new Array[Byte](buf.remaining)
      buf.get(arr, 0, arr.length);
      f(arr)
    }
    stream.close
  }
}

class Digest(alg: String) {
  private val dig = MessageDigest.getInstance(alg)
  dig.reset()
  def update(bytes: Array[Byte]) = dig.update(bytes)
  def digest() = dig.digest
  override def toString = bytes.map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
}

val dig = new Digest("sha1")
FileUtils.read(args(0), dig.update)
println(dig)
