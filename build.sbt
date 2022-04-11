ThisBuild / version := "0.0.1-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.1"

lazy val `algo-study` =
  project
    .in(file("."))
    .settings(
      name := "Algorithms & Data Structures"
    )

