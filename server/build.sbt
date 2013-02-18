name := "tvon_server"

scalaVersion := "2.10.0"

version := "1.0"

resolvers += "maven" at "http://repo1.maven.org/maven2"

resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/releases"

resolvers += "sonatype" at "http://oss.sonatype.org/content/groups/jetty"

libraryDependencies ++= Seq(
  "org.json4s"              % "json4s-native_2.10"        % "3.1.0",
  "org.scalatra"            % "scalatra_2.10"             % "2.2.0",
  "org.eclipse.jetty"       % "jetty-server"              % "9.0.0.RC0",
  "org.eclipse.jetty"       % "jetty-servlet"             % "9.0.0.RC0",
  "org.iq80.leveldb"        % "leveldb"                   % "0.5",
  "org.scala-lang"          % "scala-actors"              % "2.10.0",
  "ch.qos.logback"          % "logback-classic"           % "1.0.9",
  "net.databinder.dispatch"%% "dispatch-core"             % "0.9.5"
)

