name := "buildFirst subproject"
version := "0.1"
scalaVersion := "2.13.11"

//???? clean up (just created subproject): factor out commonality

// https://medium.com/@awesomeorji/sbt-for-the-absolute-beginner-2-settings-and-tasks-6f3b00be1a81

scalacOptions += "-deprecation"
scalacOptions += "-Ymacro-annotations"
scalacOptions += "-feature"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test,

  // Stronger types:  newtypes and refine's refinement types
  "io.estatico" %% "newtype"             % "0.4.3",
  "eu.timepit"  %% "refined"                 % "0.9.28",


  // Enumerations
  "com.beachape" %% "enumeratum" % "1.7.0",


  "org.typelevel"  %% "cats-core"           % "2.1.0",
  "org.typelevel"  %% "cats-effect"         % "2.1.0",

  "com.github.cb372" %% "cats-retry" % "2.1.0",
  "org.typelevel"  %% "squants"  % "1.6.0",

)

