name := "yaml-processor"

version := "0.1-SNAPSHOT"

organization := "sagebear"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "2.2.5" % Test,
  "commons-io" % "commons-io" % "2.4" % Test)

