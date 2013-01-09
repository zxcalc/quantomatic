name := "quanto"

version := "1.0"

scalaVersion := "2.9.2"

//libraryDependencies += "org.codehaus.jackson" % "jackson-core-asl" % "1.9.11"

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.1.2"

libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-scala" % "2.1.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.2"

EclipseKeys.withSource := true

scalacOptions ++= Seq("-unchecked", "-deprecation")

