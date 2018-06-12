import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "ru.keldysh",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "sc-mini-scala",
    libraryDependencies += parser_combinators,
    libraryDependencies += scalactic,
    libraryDependencies += scalaTest % Test,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    testOptions in Test += Tests.Argument("-oD"),
    logBuffered in Test := false,
    parallelExecution in Test := false
  )
