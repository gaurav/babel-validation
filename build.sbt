import Dependencies._

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "org.renci"
ThisBuild / organizationName := "RENCI"

lazy val root = (project in file("."))
  .settings(
    name := "BabelValidator",

    // Dependencies
    libraryDependencies += scallop,
    libraryDependencies += scalaLogging,
    libraryDependencies += logback,
    libraryDependencies += zio,
    libraryDependencies += zioStreams,
    libraryDependencies += zioJson,

    // Test dependencies
    libraryDependencies += scalaTest % Test,

    // Test settings.
    Test / logBuffered := false // Allows scalatest to do a better job with the output.
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
