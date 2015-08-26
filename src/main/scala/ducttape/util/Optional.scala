// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.util

/**
 * Convenience object for wrapping objects in an Option.
 */
object Optional {

  /**
   * Returns <code>None</code> if <code>a</code> is null,
   * <code>Some(a)</code> otherwise.
   * 
   * @param a A (possibly null) object
   */
  def toOption[A](a: A): Option[A] = if(a == null) None else Some(a)
  
}