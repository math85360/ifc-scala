name in ThisBuild := """ifc-scala"""

version in ThisBuild := "0.2-SNAPSHOT"

scalaVersion in ThisBuild := "2.11.7"

organization in ThisBuild := "iZ2Use"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

lazy val ifcscala = crossProject.in(file("."))
	.settings(
		libraryDependencies ++= Seq(
			"com.lihaoyi" %%% "utest" % "0.4.3" % "test",
			"com.lihaoyi" %%% "fastparse" % "0.3.7"
		),
		testFrameworks += new TestFramework("utest.runner.Framework")
	)
	.jsSettings(
		libraryDependencies ++= Seq(
		),
		jsDependencies ++= Seq(
		)
	)
	.jvmSettings(
		libraryDependencies ++= Seq(
		)
	)

lazy val ifcscalaJS = ifcscala.js

lazy val ifcscalaJVM = ifcscala.jvm
	
lazy val root = project.in(file(".")).aggregate(ifcscalaJS, ifcscalaJVM).
	settings(
		publish := {},
		publishLocal := {}
	)

scalacOptions in ThisBuild ++= Seq("-feature","-language:implicitConversions")