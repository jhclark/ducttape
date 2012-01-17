import javax.servlet.http._
import java.io.File

class WorkflowServlet(workflowDirs: Seq[File]) extends HttpServlet {
  import ducttape.viz._

  val db = new WorkflowDatabase("workflow.db")
  val dotFile = new File(workflowDirs.head, ".xdot") // TODO: This should actually just be .dot

  override def init() {}
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = service(req, resp)
  override def service(req: HttpServletRequest, resp: HttpServletResponse) { 
    val pw = resp.getWriter
    val dot = io.Source.fromFile(dotFile.getAbsolutePath).getLines.mkString("\n")
    val xdot = GraphViz.compileXDot(dot)
    pw.println(xdot)
    pw.flush
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

