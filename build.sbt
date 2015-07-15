name := "Subtitles"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "2.11.6",
  "org.scalaz" %% "scalaz-core" % "7.1.2",
  "org.scalaz" %% "scalaz-effect" % "7.1.2",
  "org.scalaz" %% "scalaz-typelevel" % "7.1.2",
  "com.lihaoyi" %% "fastparse" % "0.1.7",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.1.2" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

initialCommands in console := "import scalaz._, Scalaz._, fastparse._"

initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._"

