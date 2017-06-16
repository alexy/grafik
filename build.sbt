name := "grafik"

organization := "fm.wrk"

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
    "com.evernote" % "evernote-api" % "1.25.1"
    , "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
    , "io.megam" %% "newman" % "1.3.12"
    , "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"
    , "com.twitter" %% "finagle-http" % "6.25.0"
    , "org.apache.poi" % "poi" % "3.12"
    , "org.apache.poi" % "poi-ooxml" % "3.12"
    //, "org.parboiled" %% "parboiled" % "2.1.0"
    //, "com.github.tototoshi" %% "scala-csv" % "1.2.1"
    , "org.scalatest" %% "scalatest" % "2.2.2" % "test"
    , "com.lihaoyi" %% "scalatags" % "0.5.4"
    , "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided"
    , "org.json4s" %% "json4s-jackson" % "3.3.0.RC2"
    , "com.github.nscala-time" %% "nscala-time" % "2.10.0"
    )
