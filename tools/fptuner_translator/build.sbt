name := "FPTuner-Translator"

version := "0.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature")

javaOptions in run ++= Seq(
  "-Xms256M", "-Xmx2G", "-XX:+UseConcMarkSweepGC")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"