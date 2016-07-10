name := """ifc-scala"""

version := "0.1"

scalaVersion in ThisBuild := "2.11.7"

organization in ThisBuild := "iZ2Use"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

lazy val ifcscala = crossProject.in(file("."))
	.settings(
		name := "ifc-shared",
		libraryDependencies ++= Seq(
			"com.lihaoyi" %%% "utest" % "0.4.3" % "test",
			"com.lihaoyi" %%% "fastparse" % "0.3.7"
		),
		testFrameworks += new TestFramework("utest.runner.Framework")
	)
	.jsSettings(name := "ifc-js",
		libraryDependencies ++= Seq(
		),
		jsDependencies ++= Seq(
		)
	)
	.jvmSettings(name := "ifc-jvm",
		libraryDependencies ++= Seq(
		)
	)

lazy val ifcscalaJS = ifcscala.js

lazy val ifcscalaJVM = ifcscala.jvm
	
lazy val modules = project.aggregate(
//	ifcscalaJS,
	ifcscalaJVM)