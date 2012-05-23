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

  def deleteDir(dir: File) = org.apache.commons.io.FileUtils.deleteDirectory(dir)

  // Java's File.rename fails if moving between file systems
  // see http://stackoverflow.com/questions/7087743/how-to-rename-a-file-to-another-file-system
  def moveDir(src: File, dest: File) = org.apache.commons.io.FileUtils.moveDirectory(src, dest)
  def moveFile(src: File, dest: File) = org.apache.commons.io.FileUtils.moveFile(src, dest)

  def ls(dir: File): Seq[File] = {
    val listing = dir.listFiles
    if (listing == null)
      Nil
    else
      listing.toSeq
  }

  def basename(filename: String, suffix: String) = {
    if (filename.endsWith(suffix)) {
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
  
  def isGlob(path: String) = path.contains("*") || path.contains("?")
  
  def glob(pattern: String): Seq[File] = isGlob(pattern) match {
    case false => Seq(new File(pattern))
    case true => {
      // TODO: Perhaps we can find a Java implementation that behaves exactly as bash
      // but for now, just use bash.
      Shell.runGetOutputLines("ls -1 --color=no " + pattern, stdPrefix="glob", workDir=new File("."), env=Nil).
        map { str => new File(str) }
    }
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
