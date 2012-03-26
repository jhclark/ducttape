/*

package ducttape.util.args

import collection._

class ArgParser(val progName: String, preUsage: String = "", postUsage: String = "") {
  private val positions = new mutable.ArrayBuffer[Arg[_]]
  private val settings = new mutable.ArrayBuffer[Setting[_]]
  def +=[T](arg: Setting[T]): Setting[T] = {
    settings += arg
    arg
  }
  def positional[T] = {
  }
  def flag(name: String, shortName: String, desc: String) = {
    this += new Flag[T](name, Some(shortName), desc)
  }
  def flag(name: String, shortName: Option[String], desc: String) = {
    this += new Flag[T](name, shortName, desc)
  }
  def opt[T](name: String, shortName: String, valueName: String, desc: String) = {
  }
  def multiopt[T](name: String, shortName: String, valueNames: Seq[String], desc: String) = {
  }
  def usage = {
    val str = new mutable.StringBuilder
    str ++= progName + "\n"
    str ++= preUsage + "\n"
    def nameCol(name: String, short: Option[String]) = short match {
      case Some(shortName) => "%s [%s]".format(opt.name, shortName)
      case None => "%s".format(opt.name)
    }
    // TODO: Positional arguments
    for(s: Setting[_] <- settings) s match {
      // TODO: Wrap lines of long options
      case Flag => str ++= "%20s %s\n".format(nameCol(s.name, s.shortName), desc)
    }
    str ++= postUsage
    str.toString
  }
  def parse = {
    for(arg <- args) {
      err.println(arg)
    }
  }
}

trait Setting[T] {
  private[args] var value: Option[T] = None
}

trait Opt[T] extends Setting[T] {
  def apply(): Option[T] = v
}

// arguments are required to have a value, unlike options
trait Arg[T] extends Setting[T] {
  def apply(): T = v
}

// assume flags are boolean... for now
class Flag(val name: String, val shortName: Option[String], val desc: String) extends Arg[Boolean] {
}
*/
