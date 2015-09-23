// This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

package ducttape.versioner

class VersionedPackageId(val packageName: String, val packageVersion: String) {
  override def toString() = s"${packageName}/${packageVersion}"
}
