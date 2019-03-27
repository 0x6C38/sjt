import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.localKanji",
      scalaVersion := "2.12.8",
      version := "2.0"
    )),
    name := "SJT",
    crossScalaVersions := Seq("2.12.8", "2.11.8"),
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.atilika.kuromoji" % "kuromoji-ipadic" % "0.9.0",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
