name := "evo-scala-bootcamp-homework"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Ymacro-annotations"
)

val catsVersion = "2.2.0"
val circeVersion = "0.13.0"


libraryDependencies ++=Seq(
  "org.scalactic" %% "scalactic" % "3.2.0",
  "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  "org.typelevel" %% "cats-core" % catsVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % "2.4.2" % Test,
)