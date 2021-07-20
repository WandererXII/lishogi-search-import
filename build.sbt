scalaVersion := "2.13.3"
name := "lishogi-search-import"
organization := "org.lishogi"
version := "2.0"
resolvers += "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master"
libraryDependencies += "org.reactivemongo" %% "reactivemongo" % "0.20.11"
libraryDependencies += "org.reactivemongo" %% "reactivemongo-akkastream" % "0.20.11"
libraryDependencies += "org.reactivemongo"  % "reactivemongo-shaded-native" % "0.20.11-linux-x86-64"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.30"
libraryDependencies += "com.github.ornicar" %% "scalalib" % "6.8"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.5"
libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.6.5"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "joda-time" % "joda-time" % "2.10.6"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.0"
libraryDependencies += "com.typesafe.play" %% "play-ahc-ws-standalone" % "2.1.2"
libraryDependencies += "com.typesafe.play" %% "play-ws-standalone-json" % "2.1.2"

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.specs2"             %% "specs2-core"              % "4.7.0" % "test",
  "org.specs2"             %% "specs2-scalaz"            % "4.7.0" % "test",
)

resolvers ++= Seq(
  "Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases"
)

scalacOptions ++= Seq(
    "-deprecation",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-feature",
)

ThisBuild / assemblyMergeStrategy  := {
  case PathList("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("reflection-config.json") => MergeStrategy.rename
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}
