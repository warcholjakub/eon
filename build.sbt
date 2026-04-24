ThisBuild / scalaVersion := "3.8.3"
ThisBuild / organization := "dev.eon"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = project
  .in(file("."))
  .aggregate(core, cli)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "epidemic-core",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )

lazy val cli = project
  .in(file("cli"))
  .dependsOn(core)
  .settings(
    name := "epidemic-cli",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "4.1.0",
      "com.typesafe" % "config" % "1.4.3",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
