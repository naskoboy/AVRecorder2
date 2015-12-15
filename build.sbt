import sbt.Keys._
//import AssemblyKeys._ // put this at the top of the file
assemblySettings

name := "AVRecorder2"
version := "1.0"
scalaVersion := "2.11.7"

libraryDependencies ++= {
  val sprayVersion = "1.3.3"
  val akkaVersion = "2.3.14"
  Seq(
    "joda-time" % "joda-time" % "2.6",
    "javax.mail" % "mail" % "1.4.7",
    "com.typesafe" % "config" % "1.3.0",
    "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1",
    "org" % "jaudiotagger" % "2.0.3",
    //"log4j" % "log4j" % "1.2.14",
    "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "org.slf4j" % "slf4j-simple" % "1.7.12"

    //"org.scala-lang.modules" % "scala-xml_2.11" % "1.0.3"
    //"org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
    //"net.databinder.dispatch" % "dispatch-core_2.11" % "0.11.2"
    //"com.mpatric" % "mp3agic" % "0.8.3"
  )
}
