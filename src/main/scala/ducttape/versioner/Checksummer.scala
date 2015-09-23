// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.versioner

import ducttape.exec.TaskEnvironment
import java.io.File
import java.io.FileInputStream
import java.nio.MappedByteBuffer
import java.nio.channels.FileChannel.MapMode
import java.security.MessageDigest
import scala.collection.mutable.HashMap
import scala.io.Source

class Checksummer(val file:File, val messageDigestAlgorithm:String="MD5", val encoding:String="UTF8") {

  class Checksum(val filesize:Long, val timestamp:Long, val messageDigest:String) {
    
    override val hashCode:Int = messageDigest.hashCode
    
    override def equals(that:Any) = {
      that match { 
        case other:Checksum => (this.messageDigest == other.messageDigest)
        case _ => false
      }
    }
    
    override val toString = new java.util.Date(timestamp).toString + "\t" + ducttape.util.Files.humanReadableSize(filesize) + "\t" + messageDigestAlgorithm + ": " + messageDigest
  }
  
  val algorithm = MessageDigest.getInstance(messageDigestAlgorithm)
  
  val map = new HashMap[String,Checksum] 

  // Read data from file
  {
    val source = Source.fromFile(file,encoding) 
    
    try {
      for (line:String <- source.getLines) {
        line.split("\t") match {
        case Array(filename,sizeString,timeString,messageDigest) => {
          map.put(filename,new Checksum(sizeString.toLong,timeString.toLong,messageDigest))
        }
        case _ => new RuntimeException("Checksum file " + file + " contains a malformatted line:\n" + line)
        }
      }
    } finally {
      source.close
    }
    
    
  }
  
  /**
   * Returns <code>None</code> if the file size and timestamp 
   * for the specified file match that recorded in this object's cache,
   * or if the message digest of the specified file matches that 
   * recorded in this object's cache; 
   * otherwise, returns an updated Checksum.
   */
  def updateChecksum(filename:String) : Option[Checksum] = {
    val file = new File(filename)
    val filesize = file.length
    val timestamp = file.lastModified
    
    val timeSizeUnchanged:Boolean = map.get(filename) match {
      case None => false
      case Some(cached:Checksum) => {
        if (cached.filesize==filesize && cached.timestamp==timestamp) {
          true
        } else {
          false
        }
      }
    }
    
    val checksum = 
      if (timeSizeUnchanged) {
        None
      } else {
        algorithm.reset
        val stream = new FileInputStream(filename)
        var position:Long = 0
        var sizeRemaining = filesize
        val channel = stream.getChannel
        while (position < filesize) {
          val size = if (sizeRemaining<=Integer.MAX_VALUE) sizeRemaining else { Integer.MAX_VALUE }
          val buffer = channel.map(MapMode.READ_ONLY, position, size)          
          algorithm.update(buffer)
          sizeRemaining -= size
          position += size       
        }
        val bytes = algorithm.digest
        val messageDigest = bytes.map(byte => "%02x".format(byte & 0xFF)).mkString
        Some(new Checksum(filesize,timestamp,messageDigest))
      }
    
    return checksum
  }
  
}

object Checksummer {

  def main(args: Array[String]) {
    val log = new File("/tmp/log")
    
    for (filename <- args) {
      println("File: " + filename)
                  println("\tcat " + filename + " > /dev/null")
      val beforeCat = new java.util.Date().getTime
      ducttape.util.Shell.run("cat " + filename + " > /dev/null","")
      val afterCat = new java.util.Date().getTime
      println("\t\t"+((afterCat-beforeCat)/1000.0) + " seconds")
      for (algorithm <- List("MD5","SHA1")) {
        println("\tAlgorithm: " + algorithm)
    val checksummer = new Checksummer(log,algorithm)

      val before = new java.util.Date().getTime()
      val checksum = checksummer.updateChecksum(filename)
      val after = new java.util.Date().getTime()

      val seconds = (after - before) / 1000.0

      checksum match {
          case Some(value) => println("\t\t" + seconds + " seconds\t\t" + value)
          case None => ()
      }
    }
    }
  }
  
}