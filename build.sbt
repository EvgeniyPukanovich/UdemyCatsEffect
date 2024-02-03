ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

lazy val root = (project in file("."))
  .settings(
    name := "UdemyCatsEffect"
  )

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.4"

libraryDependencies += "co.fs2" %% "fs2-core" % "3.4.0"

libraryDependencies += "co.fs2" %% "fs2-io" % "3.4.0"

libraryDependencies += "co.fs2" %% "fs2-reactive-streams" % "3.4.0"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps"
)