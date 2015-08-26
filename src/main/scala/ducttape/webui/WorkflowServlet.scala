// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.webui

import javax.servlet.http._
import java.io._

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
    System.err.println("Sending new XDot graph")
    pw.println(xdot)
    pw.flush
  }

  override def init() {}
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) = service(req, resp)
  override def service(req: HttpServletRequest, resp: HttpServletResponse) { 
    val pw = resp.getWriter
    req.getParameter("queryType") match {
      case "xdot_first" => {
        sendFile(pw)
      }
      case "xdot_update" => {
        pollFileForUpdates
        sendFile(pw)
      }
      case "db_first" => {
        // TODO
        // 1) Enumerate all tasks from DB
        // 2) JSON-ify them and push them to the webapp
      }
      case "db_update" => {
        // TODO: Wait for notification from sqlite that the db has changed
        // TODO: Send data back to user
      }
      case _ @ t => {
        throw new RuntimeException("Unknown queryType: " + t)
      }
    }
  }
}