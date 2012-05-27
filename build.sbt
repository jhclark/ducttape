name := "ducttape"

version := "0.2"

scalaVersion := "2.9.2"

docDirectory in Compile := file("scaladoc")

scalacOptions ++= Seq("-unchecked", "-deprecation")

unmanagedBase in Test <<= baseDirectory { base => base / "lib/test" }

// for time-dependent deadlock detection tests
parallelExecution in Test := false


// proguard options, for making a single distributable JAR file:
seq(ProguardPlugin.proguardSettings :_*)

proguardOptions ++= Seq(
  keepMain("Ducttape")
)

minJarPath := new File("ducttape.min.jar")

makeInJarFilter <<= (makeInJarFilter) {
  (makeInJarFilter) => {
    (file) => file match {
      // don't keep JAR signature files, since we're about to break those
      // also don't keep .txt files and .html files since those tend to conflict
      case _ => makeInJarFilter(file) + ",!META-INF/*.SF,!META-INF/*.txt,!**/*.html"
    }
  }
}
