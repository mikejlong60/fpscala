name := "fp-in-scala"
version := "0.0.1"
scalaVersion := "2.12.7"

// TODO: add unused imports
scalacOptions := Seq(
  "-encoding",
  "UTF-8",
  "-target:jvm-1.8",
  "-deprecation",
  "-feature",
  "-unchecked"
)

ensimeIgnoreScalaMismatch in LocalProject("fpscala") := true

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test

