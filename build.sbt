ThisBuild / version := "0.0.1-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val `algo-study` =
    project
        .in(file("."))
        .settings(
            name := "algorithms"
        )
