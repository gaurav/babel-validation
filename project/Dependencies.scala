import sbt._

object Dependencies {
  // Dependencies
  lazy val scallop = "org.rogach" %% "scallop" % "4.1.0"

  lazy val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"
  lazy val logback = "ch.qos.logback" % "logback-classic" % "1.2.11"

  // Testing
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.12"
}
