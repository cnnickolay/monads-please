name := "monads-please"

version := "1.0"

scalaVersion := "2.12.6"

//val scalaz = "7.3.0-M23"
val scalaz = "7.2.22"

libraryDependencies += "org.scalaz" %% "scalaz-core" % scalaz
libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % scalaz
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.10.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.10.1" % Test
libraryDependencies += "io.frees" %% "iotaz-core" % "0.3.7"
