name := "quanto"

version := "1.0"

scalaVersion := "2.9.2"

resolvers += "repo.codahale.com" at "http://repo.codahale.com"

libraryDependencies += "com.codahale" % "jerkson_2.9.1" % "0.5.0" withSources()

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M4" % "test"

EclipseKeys.withSource := true
