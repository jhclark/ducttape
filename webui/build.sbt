name := "Ducttape Web UI"

version := "0.1"

scalaVersion := "2.8.1"

libraryDependencies ++= Seq(
  "net.liftweb" %% "lift-webkit" % "2.3" % "compile",
  "org.eclipse.jetty" % "jetty-webapp" % "7.3.0.v20110203" % "jetty",
  "ch.qos.logback" % "logback-classic" % "0.9.26"
)


//libraryDependencies ++= Seq(
//    "net.liftweb" %% "lift-webkit" % liftVersion % "compile->default",
//    "org.mortbay.jetty" % "jetty" % "6.1.22" % "test->default",
//    "junit" % "junit" % "4.5" % "test->default",
//    "org.scala-tools.testing" %% "specs" % "1.6.6" % "test->default")

//defaultExcludes ~= (filter: FileFilter => filter || "*~")

seq(webSettings :_*)