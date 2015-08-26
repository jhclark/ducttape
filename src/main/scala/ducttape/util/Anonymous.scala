// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
