name := "buildFirst subproject"
version := "0.1"
scalaVersion := "2.13.11"

//???? clean up (just created subproject): factor out commonality

// https://medium.com/@awesomeorji/sbt-for-the-absolute-beginner-2-settings-and-tasks-6f3b00be1a81

scalacOptions += "-deprecation"
scalacOptions += "-Ymacro-annotations"
scalacOptions += "-feature"

libraryDependencies ++= Seq(

  "eu.timepit"  %% "refined" % "0.11.0",
)

