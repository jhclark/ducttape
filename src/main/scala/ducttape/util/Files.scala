package ducttape.util

import java.io._
import scala.io.Source
import java.net.URI
import java.net.URL

object Files {
  def write(str: String, file: File) {
    file.getParentFile().mkdirs()
    val fw = new FileWriter(file)
    try {
      fw.write(str)
    } finally {
      fw.close
    }
  }

  def writer(file: File): PrintWriter = {
    new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8")))
  }

  def writer(opt: Option[File]): PrintWriter = opt match {
    case Some(file: File) => writer(file)
    case None => new PrintWriter(NullWriter)
  }

  // reads all lines, but closes file unlike io.Source
  def read(file: File): Seq[String] = {
    val br = new BufferedReader(new FileReader(file))
    try {
      Iterator.continually(br.readLine).takeWhile(_ != null).toList
    } finally {
      br.close
    }
  }

  // there is no reliable way of detecting symlinks in Java
  // f.getAbsolutePath != f.getCanonicalPath fails since /home/./jhclark is not canonical
  def deleteDir(dir: File) {
    ApacheFileUtils.deleteDirectory(dir)
  }

  def ls(dir: File): Seq[File] = {
    val listing = dir.listFiles
    if(listing == null)
      Nil
    else
      listing.toSeq
  }

  def basename(filename: String, suffix: String) = {
    if(filename.endsWith(suffix)) {
      filename.substring(0, filename.length - suffix.length)
    } else {
      filename
    }
  }
  
  def copy(src: File, dest: File) {
    val to = new FileOutputStream(dest).getChannel
    val from = new FileInputStream(src).getChannel
    to.transferFrom(from, 0, Long.MaxValue)
  }
}

object NullWriter extends Writer {
  override def close(): Unit = Unit
  override def flush(): Unit = Unit
  override def write(cbuf: Array[Char]): Unit = Unit
  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = Unit
}

object IO {
  def read(input: Any, encoding: String) = input match {
    case bytes: Array[Byte]    => new InputStreamReader(new ByteArrayInputStream(bytes),encoding)
    case chars: Array[Char]    => new CharArrayReader(chars)
    case c: Char               => new StringReader(""+c)
    case file: File            => Source.fromFile(file, encoding).reader
    case inStream: InputStream => Source.fromInputStream(inStream,encoding).reader
    case string: String        => new StringReader(string)
    case uri: URI              => Source.fromFile(uri,encoding).reader
    case url: URL              => Source.fromURL(url,encoding).reader
    case any: AnyRef           => throw new RuntimeException("I don't know how to parse objects of type " + any.getClass())
    case _                     => throw new RuntimeException("I don't know how to parse objects of that type")
  }
}
