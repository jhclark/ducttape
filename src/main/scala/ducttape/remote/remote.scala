/*
package ducttape.remote

import java.io._
import java.net.{InetAddress,ServerSocket,Socket,SocketException}

import collection._

import ducttape.util._

@serializable class RemoteRequest(val str: String) {
  override def toString = str
}
@serializable class RemoteResponse(val map: Map[String,String]) {
  override def toString = map.toString
}

object RemoteClient {
  def main(args: Array[String]) {
    try {
      val port = Integer.parseInt(args(0))
      val socket = new Socket(InetAddress.getByName("localhost"), port)
      val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream))
      val in = new ObjectInputStream(new DataInputStream(socket.getInputStream))
      
      val request = new RemoteRequest("basic status")
      out.writeObject(request)
      out.flush

      while(true) {
        val x = in.readObject.asInstanceOf[RemoteResponse]
        println(x)
      }
      out.close
      in.close
      socket.close
    } catch {
      case e: IOException => e.printStackTrace
    }
  }
}

object RemoteServer {
  def status(statusType: String): Map[String,String] = {
    import sys.process._
    val map = new mutable.HashMap[String,String]
    map += "DiskUsage" -> Shell.runGetOutput("df -Ph . | awk 'END{print $5}'")
    map += "JobsInQstat" -> Shell.runGetOutput("qstat -u '*' | wc -l")
    map += "JobRunningOnHadoop" -> Shell.runGetOutput("hadoop job -list | fgrep 'jobs currently running'")
    map
  }

  def main(args: Array[String]) {
    val port = Integer.parseInt(args(0))
    try {
      val listener = new ServerSocket(port)
      while(true) new ServerThread(listener.accept()).start()
      listener.close
    } catch {
      case e: IOException => e.printStackTrace; System.exit(1)
    }
  }
}

class ServerThread(socket: Socket) extends Thread("ServerThread") {
  override def run() {
    try {
      val out = new ObjectOutputStream(new DataOutputStream(socket.getOutputStream))
      val in = new ObjectInputStream(new DataInputStream(socket.getInputStream))
      while(true) {
        val request = in.readObject.asInstanceOf[RemoteRequest]
        val map = RemoteServer.status(request.str)
        out.writeObject(new RemoteResponse(map))
        out.flush
      }
    } catch {
      case e: SocketException => e.printStackTrace
      case e: IOException => e.printStackTrace
    }
  }
}
*/
