name := "ducttape"

version := "0.2"

scalaVersion := "2.9.2"

docDirectory in Compile := file("scaladoc")

scalacOptions ++= Seq("-unchecked", "-deprecation")
