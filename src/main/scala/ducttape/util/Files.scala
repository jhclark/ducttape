package ducttape.util

import java.io._
import scala.io.Source
import java.net.URI
import java.net.URL
import annotation.tailrec

import grizzled.slf4j.Logging

object Files extends Logging {
  
  def humanReadableSize(bytes:Long) = {
    val units = List("B","KB","MB","GB","TB","PB","EB","ZB","YB")
    var result = BigDecimal.long2bigDecimal(bytes)
    
    if (bytes < 0) result *= -1
    
    var unitIndex = 0
    
    while (unitIndex < units.length && result >= 1024) {
      result /= 1024.0
      unitIndex += 1
    }
    
    result.toString + " " + units(unitIndex)
  }
  
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

  // Java's File.rename fails if moving between file systems
  // see http://stackoverflow.com/questions/7087743/how-to-rename-a-file-to-another-file-system
  // ..and Apache Commons IO is incapable of deleting/moving directories containing broken symlinks

  //def deleteDir(dir: File) = org.apache.commons.io.FileUtils.deleteDirectory(dir)
  def deleteDir(dir: File) {
    // note: we ignore the exit code in case nothing was removed
    Shell.run("rm -rf %s".format(dir.getAbsolutePath), "deleteDir")
  }

  //org.apache.commons.io.FileUtils.moveDirectory(src, dest)
  def moveDir(src: File, dest: File) {
    dest.getParentFile.mkdirs()
    val result = Shell.run("mv %s %s".format(src.getAbsolutePath, dest.getAbsolutePath), "moveDir")
    if (result != 0) {
      throw new RuntimeException("Failed to move %s to %s".format(src.getAbsolutePath, dest.getAbsolutePath))
    }
  }
  def moveFile(src: File, dest: File) = org.apache.commons.io.FileUtils.moveFile(src, dest)
  def moveFileToDir(src: File, dest: File, createDestDir: Boolean = true)
    = org.apache.commons.io.FileUtils.moveFileToDirectory(src, dest, createDestDir)
  
  // returns path, relative to the relativeToDir
  def relativize(path: File, baseDir: File): String = {
    val absPath = path.getAbsoluteFile
    val absBaseDir = baseDir.getAbsoluteFile
    absBaseDir.toURI.relativize(absPath.toURI).getPath
  }
    
  def symlink(pointTo: File, link: File) {
    // note: we ignore the exit code in case nothing was linked
    // absolute symlink paths are *evil* since they mean the directory can never be relocated
    val absLinkDir: File = link.getAbsoluteFile.getParentFile
    val relativePointTo: String = relativize(pointTo, absLinkDir)
    val linkName = link.getName
    debug("Relativized %s relative to %s: %s".format(pointTo, link, relativePointTo))
    debug("Symlinking to %s in %s".format(relativePointTo, absLinkDir))
    Shell.run("cd %s && ln -sf %s %s".format(absLinkDir.getAbsolutePath, relativePointTo, linkName), stdPrefix="ln")
  }

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
  
  def copyToDir(src: File, destDir: File) = copy(src, new File(destDir, src.getName))

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
  
  def mkdirs(dir: File) {
    dir.mkdirs()
    if (!dir.exists) {
      throw new IOException("Could not create directory: " + dir.getAbsolutePath)
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
