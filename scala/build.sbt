ThisBuild / version := "1.0.0"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "com.dsa"

lazy val root = (project in file("."))
  .settings(
    name := "dsa-scala",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    )
  )
