name := "quanto"

version := "1.0"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-feature", "-language:implicitConversions")

retrieveManaged := true

//seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.1" withSources() withJavadoc()

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.1.2"

libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-scala" % "2.1.2"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"

libraryDependencies += "org.scala-lang" % "scala-swing" % scalaVersion.value

//EclipseKeys.withSource := true

//exportJars := true

seq(appbundle.settings: _*)

appbundle.mainClass := Some("quanto.gui.QuantoDerive")

appbundle.javaVersion := "1.6+"

appbundle.screenMenu := true

appbundle.name := "QuantoDerive"

appbundle.normalizedName := "quantoderiveapp"

appbundle.organization := "org.quantomatic"

appbundle.version := "0.2.0"

appbundle.icon := Some(file("../docs/graphics/quantoderive.icns"))


scalacOptions ++= Seq("-unchecked", "-deprecation")

