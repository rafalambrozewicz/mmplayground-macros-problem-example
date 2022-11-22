ThisBuild / organization := "com.softwaremill.mmplayground"
ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.0.1"

lazy val mymacros = (project in file("mymacros")).settings(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % "2.13.8",
  )
)

lazy val myapp = (project in file("myapp")).dependsOn(mymacros).settings(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.14" % Test,
  )
)

lazy val root = (project in file(".")).aggregate(mymacros, myapp)