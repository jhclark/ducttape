package ducttape.syntax

import ducttape.util.Strings

/** Represents a task name along with all of the groups that
 *  contain it.
 *  ancestry is delimited by forward slashes without a leading nor trailing slash
 */
class Namespace(val name: String, val ancestry: Option[String] = None) {
    
  override def equals(that: Any) = {
    that match { 
      case other: Namespace => (other.toString == this.toString)
      case _                => false
    }
  }
  
  // TODO: More smearing
  override def hashCode() = name.hashCode ^ ancestry.hashCode
    
  /** Note: This is used by CLI globbing, so this representation must stay consistent */
  override def toString() = ancestry match {
    case Some(groupNames: String) => s"${name}/${groupNames}"
    case None                     => name
  }
}

object Namespace {
  def fromString(str: String) = {
    val (groupNames: Option[String], taskName: String) = Strings.splitOnLast(str, '/')
    new Namespace(taskName, groupNames)
  }
}
