// surviving sbt 0.10: http://eed3si9n.com/sbt-010-guide

// ProGuard, for making a single, tiny JAR file
seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("Ducttape")

// end ProGuard

name := "Ducttape"

version := "0.1"

scalaVersion := "2.9.0"

//defaultExcludes ~= (filter: FileFilter => filter || "*~")

//scalaSource in Compile := file("scala")

docDirectory in Compile := file("scaladoc")

unmanagedBase := file("lib")

scalacOptions ++= Seq("-unchecked", "-deprecation")

mainClass in (Compile, packageBin) := Some("Ducttape")

mainClass in (Compile, run) := Some("Ducttape")

