name := "evo-scala-bootcamp-homework"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations"
)

val http4sVersion = "0.21.7"
val catsVersion = "2.2.0"
val catsEffectVersion = "2.2.0"
val circeVersion = "0.13.0"
val log4CatsVersion = "1.1.1"


libraryDependencies ++=Seq(
  "org.scalactic" %% "scalactic" % "3.2.0",
  "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  "io.chrisdavenport" %% "log4cats-slf4j" % log4CatsVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % "2.4.2" % Test,
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
