version := "0.0.1"

organization := "tupol"

name := "scala-patterns"

fork := true

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation", "-feature")


libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.5" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

