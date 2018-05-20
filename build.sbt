name := "monads-please"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.22"
libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.22"
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.10.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.10.1" % Test
libraryDependencies += "io.frees" %% "iotaz-core" % "0.3.7"
