name := "Parser"

version := "0.1"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.26",
  "org.specs2" %% "specs2-core" % "4.3.4" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

scalaVersion := "2.12.7"