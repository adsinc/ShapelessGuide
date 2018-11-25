import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Shapeless Experiments",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.3",
      scalaTest % Test
    ),
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    )
  )

