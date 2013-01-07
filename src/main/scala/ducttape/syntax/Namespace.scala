package ducttape.syntax

  
class Namespace(val name:String, val ancestry:Option[String] = None) {
    
  override def equals(that: Any) = {
    that match { 
      case other: Namespace => (other.toString == this.toString)
      case _                => false
    }
  }
    
  override def hashCode = toString.hashCode
    
  override def toString = {
    ancestry match {
      case Some(s:String) => name + "/" + s
      case None           => name
    }
  }
    
}

