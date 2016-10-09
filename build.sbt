scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

initialCommands in console := """
  import functors._
  import adjoints._
  import adjoints.pets._
  import Adjoint.Syntax
  """
