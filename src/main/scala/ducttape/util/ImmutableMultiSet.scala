// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.util

trait ImmutableMultiSet[A] {
  def find(func: A => Boolean): Option[A]
  def removeAll(a: A)
  def apply(a: A)
  def contains(a: A)
  def keys()
  def view()
  def toList(): List[A]
}