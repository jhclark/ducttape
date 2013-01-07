package ducttape.util

object Booleans {
  def parseBoolean(str: String): Boolean = str.toLowerCase match {
    case "true" => true
    case "false" => false

    case "1" => true
    case "0" => false

    case "enable" => true
    case "disable" => false
    
    case "t" => true
    case "f" => false
  }
}
