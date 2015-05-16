name := "sever"

organization := "io.bythebay"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
    "com.evernote" % "evernote-api" % "1.25.1"
    , "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
    , "io.megam" %% "newman" % "1.3.8"
    , "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"
    , "com.twitter" %% "finagle-http" % "6.25.0"
    //, "org.parboiled" %% "parboiled" % "2.1.0"
    //, "com.github.tototoshi" %% "scala-csv" % "1.2.1"
    , "org.scalatest" %% "scalatest" % "2.2.2" % "test"
    )
