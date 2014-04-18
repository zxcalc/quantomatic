name := "quanto"

version := "1.0"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-feature", "-language:implicitConversions")

//seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.1" withSources() withJavadoc()

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.1.2"

libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-scala" % "2.1.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

//libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.0"

EclipseKeys.withSource := true

//exportJars := true

mainClass := Some("quanto.gui.QuantoDerive")

seq(appbundle.settings: _*)

appbundle.javaVersion := "1.6+"

appbundle.screenMenu := false

appbundle.name := "QuantoDerive"

appbundle.normalizedName := "quantoderiveapp"

appbundle.organization := "org.quantomatic"

appbundle.version := "0.2.0"

appbundle.icon := Some(file("../docs/graphics/quantoderive.icns"))


scalacOptions ++= Seq("-unchecked", "-deprecation")

