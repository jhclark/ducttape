package ducttape.util

import scala.collection.mutable.HashMap

/**
 * HashMap where each key is associated with an initially empty [[scala.collection.mutable.StringBuilder]]
 */
class GlobValuesMap extends HashMap[String,StringBuilder] {

  /**
   * Gets a new StringBuilder for the specified key,
   * and puts this new value in the map for that key.
   */
  override def default(key: String) : StringBuilder = {
      val v = new StringBuilder
      put(key,v)
      return v
  }  

  /**
   * Appends the specified value to the StringBuilder
   * associated with this key.
   * <p>
   * Values inserted into the StringBuilder using this method
   * will be separated by whitespace.
   */
  def addValue(key: String, value: String) {
      val inputValues = this(key)
      if (! inputValues.isEmpty) {
        inputValues.append(' ')
      }
      inputValues.append(value)
  }
  
  /**
   * Converts this to a sequence of key, value tuples,
   * where the value is the String value of the StringBuilder
   */
  def toStringTupleSeq() : Seq[(String,String)] = {
    return this.toSeq.map {
      tuple:(String,StringBuilder) => { (tuple._1, tuple._2.toString) }
    }
  }
  
}
