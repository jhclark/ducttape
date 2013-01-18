package ducttape.versioner

class VersionedPackageId(val packageName: String, val packageVersion: String) {
  override def toString() = s"${packageName}/${packageVersion}"
}
