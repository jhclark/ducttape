package ducttape.util

abstract class Anonymous[Contents] {

	private val map = new java.util.IdentityHashMap[Contents,String]

  def anonymousString(number:Int) : String

  def lookupName(of:Contents) : Option[String]

	def getName(contents:Contents) : String = {
			return lookupName(contents) match {
			case Some(name:String) => name
			case None              => {
				if (map.containsKey(contents)) {
					map.get(contents)
				} else {
					val name = anonymousString(map.size)
							map.put(contents, name)
							name
				}
			}
			}
	}
}
