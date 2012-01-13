import javax.servlet.http._

class WorkflowServlet extends HttpServlet {
  val db = new WorkflowDatabase("workflow.db")
  override def init() {}
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = service(req, resp)
  override def service(req: HttpServletRequest, resp: HttpServletResponse) { 
    val pw = resp.getWriter
    // TODO: First run "dot -Txdot" to get xdot from graphviz
    val xdot = """
digraph hello_world {
  node [label="\N"];
    graph [bb="0,0,66,108",
      _draw_="c 9 -#ffffffff C 9 -#ffffffff P 4 0 -1 0 108 67 108 67 -1 ",
      xdotversion="1.2"];
    hello [href="javascript:alert('ohai')", pos="33,90", width="0.83333", height="0.5", _draw_="c 9 -#000000ff e 33 90 30 18 ", _ldraw_="F 14.000000 11 -Times-Roman c 9 -#000000ff T 33 84 0 31 5 -hello "];
  world [pos="33,18", width="0.91667", height="0.5", _draw_="c 9 -#000000ff e 33 18 33 18 ", _ldraw_="F 14.000000 11 -Times-Roman c 9 -#000000ff T 33 12 0 35 5 -world "];
  hello -> world [pos="e,33,36.413 33,71.831 33,64.131 33,54.974 33,46.417", _draw_="c 9 -#000000ff B 4 33 72 33 64 33 55 33 46 ", _hdraw_="S 5 -solid c 9 -#000000ff C 9 -#000000ff P 3 37 46 33 36 30 46 "];
}
"""
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

