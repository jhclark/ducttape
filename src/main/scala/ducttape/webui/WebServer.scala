// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.webui

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler._
import org.eclipse.jetty.server.bio.SocketConnector
import org.eclipse.jetty.servlet._
import javax.servlet._
import java.io.File
import ducttape.util.Files

object WebServer extends App {
  
  if (args.length != 2) {
    System.err.println("Usage: webui.sh port workflowsFile")
    System.exit(1)
  }
  
  val port = args(0).toInt
  val workflowsFile = args(1)
  val server = new Server(port)

  val workflows = Files.read(new File(workflowsFile)).map(str => new File(str))

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

