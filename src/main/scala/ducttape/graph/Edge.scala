package ducttape.graph

/**
 * @author Lane Schwartz <dowobeha@gmail.com>
 */
class Edge(val from: Vertex, val to: Vertex, val contents:Option[Any]=None, val comment: Option[String]=None) {

  if (from==null) throw new NullPointerException("Value from was initialized as null. This is forbidden.")
  if (to==null) throw new NullPointerException("Value from was initialized as null. This is forbidden.")

  // Calculate the hash code once
  private lazy val cachedHashCode = {
    from.hashCode() +
    31*to.hashCode()
//   + 31*(contents match {
//      case None => 0
//      case Some(c) => c.hashCode()
//    })
  }

  override def hashCode() = cachedHashCode

  override def equals(that: Any) : Boolean = {
    that match {
      case other: Edge => {
        if (other.from.equals(from) && other.to.equals(to)) {
          return true
        } else {
          return false
        }
      }
      case _ => return false
    }
  }

  override def toString() = comment match {

    // If we have a comment, use it
    case Some(str) => "%s->%s:\t%s".format(from,to,str)

    case None => contents match{

      // If we have content, but no comment, just use the comment
      case Some(contents) => "%s->%s:\t%s".format(from,to,contents.toString)

      // If we have no comment and no content, just use the ID
      case None => "%s->%s".format(from,to)

    }

  }
}
