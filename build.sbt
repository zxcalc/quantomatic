name := "quanto"

version := "1.0"

scalaVersion := "2.12.6"

scalacOptions ++= Seq("-feature", "-language:implicitConversions")

retrieveManaged := true

fork := true

//seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.12" withSources() withJavadoc()

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.9.5"

libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.5"

//libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

//libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.3"

//libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"

//EclipseKeys.withSource := true

//exportJars := true

seq(appbundle.settings: _*)

appbundle.mainClass := Some("quanto.gui.QuantoDerive")

appbundle.javaVersion := "1.8+"

appbundle.screenMenu := true

appbundle.name := "QuantoDerive"

appbundle.normalizedName := "quantoderiveapp"

appbundle.organization := "org.quantomatic"

appbundle.version := "0.3.0"

appbundle.icon := Some(file("../docs/graphics/quantoderive.icns"))

test in assembly := {}

assemblyJarName in assembly := "Quantomatic.jar"

mainClass in assembly := Some("quanto.gui.QuantoDerive")


scalacOptions ++= Seq("-unchecked", "-deprecation")

