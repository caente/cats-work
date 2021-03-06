scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.14.0",
  "org.typelevel" %% "cats" % "0.7.2",
  "com.github.mpilquist" %% "simulacrum" % "0.8.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "com.lihaoyi" % "ammonite" % "0.7.8" % "test" cross CrossVersion.full
)


initialCommands in (Test,console) := s"""
 ammonite.Main().run(
   "a" -> 1
)
"""

initialCommands in console := """
  import meetings._
  import Intention._
  import cats._
  import cats.syntax.eq._
  import cats.syntax.functor._
  import cats.instances.all._
  import common.graph._
  """
