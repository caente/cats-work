scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.14.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

initialCommands in console := """
  import functors._
  import adjoints._
  import adjoints.pets._
  import Adjoint.Syntax
  """
