name := "PSO"

version := "0.1"

scalaVersion := "2.10.4"

libraryDependencies += "au.com.bytecode" % "opencsv" % "2.4"


libraryDependencies += "org.apache.spark" %% "spark-core" % "1.6.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.rogach" %% "scallop" % "2.0.7")

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.4.0"

libraryDependencies += "com.lambdaworks" %% "jacks" % "2.3.3"