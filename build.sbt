name := "PSO"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies += "au.com.bytecode" % "opencsv" % "2.4"


libraryDependencies += "org.apache.spark" %% "spark-core" % "1.6.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.rogach" %% "scallop" % "3.4.0")

libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.4.0"

libraryDependencies += "com.lambdaworks" %% "jacks" % "2.3.3"

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}