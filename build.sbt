name := "fuzzy-sets"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test