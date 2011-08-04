package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._


/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("code")

    // Build SiteMap
    val entries = List(
      Menu.i("Home") / "index" // the simple way to declare a menu

      // more complex because this menu allows anything in the
      // /static path to be visible
      // TODO: This would be a good place for a built-in help page
      //Menu(Loc("Static", Link(List("static"), true, "/static/index"), 
	//       "Static Content"))
    )

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries:_*))

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // TODO: Move this elsewhere...
    import concurrent.ops._
    spawn {
      while(true) {
        import code.comet._
        StatusServer ! "Things are happening..."
        Thread.sleep(3000)
      }
    }
  }
}
