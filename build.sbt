// surviving sbt 0.10: http://eed3si9n.com/sbt-010-guide

name := "Ducttape Web UI"

version := "0.1"

scalaVersion := "2.9.0"

//defaultExcludes ~= (filter: FileFilter => filter || "*~")

scalaSource in Compile := file("scala")

docDirectory in Compile := file("scaladoc")

unmanagedBase := file("lib")

