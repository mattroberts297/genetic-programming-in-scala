lazy val root = (project in file(".")).settings(
  name := "genetic-programming-1",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.7",
  scalacOptions := Seq("-deprecation", "-feature", "-unchecked"),
  libraryDependencies ++= Seq(
    "com.typesafe" % "config" % "1.3.0",
    "org.slf4s" %% "slf4s-api" % "1.7.12",
    "ch.qos.logback" % "logback-classic" % "1.1.3" % "runtime",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
    "org.mockito" % "mockito-all" % "1.10.19" % "test"
  )
)
