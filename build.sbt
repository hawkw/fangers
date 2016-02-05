resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

name := "fangers"
organization := "me.hawkweisman"
version := "0.0.1"
scalaVersion := "2.11.7"
val scalatestVersion = "2.2.6"
libraryDependencies ++= Seq(
    "org.scalactic"      %% "scalactic"  % scalatestVersion
  , "org.scalacheck"    %% "scalacheck" % "1.12.2"          % "test"
  , "org.scalatest"     %% "scalatest"  % scalatestVersion  % "test"
  )
