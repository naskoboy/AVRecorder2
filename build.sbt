import sbt.Keys._

lazy val root = Project("avrecorder2", file(".")).enablePlugins(SbtTwirl)
//sourceDirectories in (Compile, TwirlKeys.compileTemplates) := (unmanagedSourceDirectories in Compile).value

name := "AVRecorder2"
version := "1.0"
scalaVersion := "2.11.8"

libraryDependencies ++= {
  val sprayVersion = "1.3.3"
  val akkaVersion = "2.3.14"
  Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "joda-time" % "joda-time" % "2.6",
    "javax.mail" % "javax.mail-api" % "1.5.5",
    //"javax.activation" % "activation" % "1.1.1",
    //"javax.mail" % "mail" % "1.4.7",
    //"javax.mail" % "javax.mail" % "1.5.5",
    "com.sun.mail" % "javax.mail" % "1.5.5",
  "com.typesafe" % "config" % "1.3.0",
    "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1",
    "org" % "jaudiotagger" % "2.0.3",
    //"log4j" % "log4j" % "1.2.14",
    "org.scala-lang.modules" % "scala-xml_2.11" % "1.0.5",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "org.slf4j" % "slf4j-simple" % "1.7.12",
    "io.spray" % "spray-can_2.11" % sprayVersion,
    "io.spray" % "spray-routing_2.11" % sprayVersion,
    "io.spray" % "spray-testkit_2.11" % sprayVersion,
    "io.spray" % "spray-client_2.11" % sprayVersion,
    "com.github.t3hnar" % "scala-bcrypt_2.11" % "2.5",
    "com.google.api-client" % "google-api-client" % "1.21.0"

    //"org.scala-lang.modules" % "scala-xml_2.11" % "1.0.3"
    //"org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
    //"net.databinder.dispatch" % "dispatch-core_2.11" % "0.11.2"
    //"com.mpatric" % "mp3agic" % "0.8.3"
  )
}

val deployTask = TaskKey[Unit]("deploy", "Copies assembly jar to remote location")
// http://blog.bstpierre.org/writing-simple-sbt-task
// http://blog.byjean.eu/2015/07/10/painless-release-with-sbt.html
// https://eknet.org/main/dev/sbt-create-distribution-zip.html
deployTask <<=  assembly map { asm =>
  val distdir = new File("""R:\target""")
  val remote = distdir / asm.getName
  println(s"Copying assembly into $remote")
  IO.copyFile(asm, remote)
}
