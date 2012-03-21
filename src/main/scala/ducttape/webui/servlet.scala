import javax.servlet.http._
import java.io._
import System._

class WorkflowServlet(workflowDirs: Seq[File]) extends HttpServlet {
  import ducttape.viz._
  import ducttape.util._

  //val db = new WorkflowDatabase("workflow.db")
  val dotFile = new File(workflowDirs.head, ".xdot") // TODO: This should actually just be .dot
  val timeout = 15 // poll file every N secs

  var lastChanged: Long = 0 // unix timestamp
  def pollFileForUpdates() {
    while(lastChanged == dotFile.lastModified) {
      Thread.sleep(timeout*1000)
    }
    lastChanged = dotFile.lastModified
    System.err.println("Detected file change")
  }

  def sendFile(pw: PrintWriter) {
    val dot = Files.read(dotFile).mkString("\n")
    val xdot = GraphViz.compileXDot(dot)
    err.println("Sending new XDot graph")
    pw.println(xdot)
    pw.flush
  }

  override def init() {}
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = service(req, resp)
  override def service(req: HttpServletRequest, resp: HttpServletResponse) { 
    val pw = resp.getWriter
    req.getParameter("queryType") match {
      case "first" => {
        sendFile(pw)
      }
      case "update" => {
        pollFileForUpdates
        sendFile(pw)
      }
      case _ @ t => {
        throw new RuntimeException("Unknown queryType: " + t)
      }
    }
  }
}

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler._
import org.eclipse.jetty.server.bio.SocketConnector
import org.eclipse.jetty.servlet._
import javax.servlet._

object WebServer extends App {
  val port = args(0).toInt
  val workflowsFile = args(1)
  val server = new Server(port)

  val workflows = io.Source.fromFile(workflowsFile).getLines.toList.map(str => new File(str))

  val sc = new ServletContextHandler(hl, "/json", false, false)
  val servlet: Servlet = new WorkflowServlet(workflows)
  sc.addServlet(new ServletHolder(servlet), "/")

  val res = new ResourceHandler
  res.setDirectoriesListed(true)
  res.setWelcomeFiles(Array("index.htm"))
  res.setResourceBase("webui/")

  val hl = new HandlerList
  hl.setHandlers(Array(sc, res, new DefaultHandler))
  server.setHandler(hl)
  
  server.start

  //def filter(filt: Filter) {
  //  val holder = new FilterHolder(filt)
  //  current.addFilter(holder, "/*", FilterMapping.DEFAULT)
  //}
}

