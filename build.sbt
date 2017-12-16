name := "haruko"

version := "1.0"

scalaVersion := "2.11.8"

enablePlugins(PlayScala)
enablePlugins(BuildInfoPlugin)

buildInfoKeys := Seq(name, version, scalaVersion, sbtVersion)
buildInfoPackage := "haruko"

resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.jcenterRepo,
  Resolver.sonatypeRepo("public")
)

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test,
  "com.jsuereth" %% "scala-arm" % "2.0",
  "org.feijoas" %% "mango" % "0.13",
  "net.dv8tion" % "JDA" % "3.3.1_303",
  "net.dean.jraw" % "JRAW" % "0.9.0",
  "org.pac4j" % "play-pac4j" % "3.0.0",
  "org.pac4j" % "pac4j-oauth" % "2.0.0",
  "com.typesafe.play" %% "play-slick" % "2.0.0",
  //"com.typesafe.play" %% "play-slick-evolutions" % "2.0.0",
  "org.postgresql" % "postgresql" % "42.1.4",

  // Verification features:
  "com.blueconic" % "browscap-java" % "1.2.0",

  // Search features:
  "net.ruippeixotog" %% "scala-scraper" % "2.0.0-RC2",
  "com.google.apis" % "google-api-services-customsearch" % "v1-rev57-1.23.0"
)
