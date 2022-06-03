// For building an uberJAR that contains all necessary libraries.
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.2.0")

// For scalafmt syntax standardization.
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")

// For scalafix code linting and validation.
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.10.0")
