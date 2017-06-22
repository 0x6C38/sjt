import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "SJT",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.atilika.kuromoji" % "kuromoji-ipadic" % "0.9.0"
  )
