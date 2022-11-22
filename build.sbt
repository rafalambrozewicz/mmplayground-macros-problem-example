ThisBuild / organization := "com.softwaremill.mmplayground"
ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.0.1"

lazy val mymacros = (project in file("mymacros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.8",
)

lazy val myapp = (project in file("myapp")).dependsOn(mymacros)

lazy val root = (project in file(".")).aggregate(mymacros, myapp)