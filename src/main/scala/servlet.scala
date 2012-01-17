import javax.servlet.http._

class WorkflowServlet extends HttpServlet {
  val db = new WorkflowDatabase("workflow.db")
  override def init() {}
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = service(req, resp)
  override def service(req: HttpServletRequest, resp: HttpServletResponse) { 
    val pw = resp.getWriter
    // TODO: Compile XDot
    val xdot = io.Source.fromFile("webui2/xdot").getLines.mkString("\n")
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
  val port = 8080
  val server = new Server(port)

  val sc = new ServletContextHandler(hl, "/json", false, false)
  sc.addServlet(new ServletHolder(classOf[WorkflowServlet]), "/")

  val res = new ResourceHandler
  res.setDirectoriesListed(true)
  res.setWelcomeFiles(Array("index.htm"))
  res.setResourceBase("webui2/")

  val hl = new HandlerList
  hl.setHandlers(Array(sc, res, new DefaultHandler))
  server.setHandler(hl)
  
  server.start

  //def filter(filt: Filter) {
  //  val holder = new FilterHolder(filt)
  //  current.addFilter(holder, "/*", FilterMapping.DEFAULT)
  //}
}

