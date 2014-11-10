name := "sudoku"

scalaVersion := "2.11.2"

libraryDependencies += "edu.umass.cs" %% "cmpsci220" % "1.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

parallelExecution in Test := false

fork in Test := true