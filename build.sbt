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
  keepMain("Ducttape"),
  //
  // keep dynamically bound logger implementation
  "-keep class org.slf4j.impl.StaticLoggerBinder",
  "-keep class org.slf4j.impl.StaticMarkerBinder",
  "-keep class org.slf4j.impl.StaticMDCBinder",
  //
  // shut up about classes that do introspection safely
  "-dontnote", "org.apache.commons.lang3.ObjectUtils",
  "-dontnote", "org.eclipse.jetty.jndi.DataSourceCloser",
  "-dontnote", "org.eclipse.jetty.util.TypeUtil",
  "-dontnote", "org.eclipse.jetty.util.log.Log",
  "-dontnote", "org.eclipse.jetty.util.log.LoggerLog",
  //
  // preserve enums
  """-keepclassmembers enum * {
    public static **[] values();
    public static ** valueOf(java.lang.String);
  }
  """
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
