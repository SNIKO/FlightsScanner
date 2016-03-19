
assemblyJarName in assembly := "scanner.jar"

name := "FlightsScanner"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies += "com.typesafe" % "config" % "1.3.0"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.0.4"

libraryDependencies += "com.typesafe.akka" %% "akka-http-experimental" % "2.0-M1"

libraryDependencies += "com.typesafe.akka" %% "akka-stream-experimental" % "2.0-M1"