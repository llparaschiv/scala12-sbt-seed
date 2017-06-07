import com.typesafe.sbt.SbtScalariform

organization := "com.vicpara"

name := "pdf-qa"

releaseVersionFile := file("version.sbt")

scalaVersion := "2.12.2"

crossScalaVersions := Seq("2.11.8")

scalacOptions ++= List("-deprecation", "-feature")

autoScalaLibrary := false

ivyScala := ivyScala.value.map(_.copy(overrideScalaVersion = true))

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("-SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

libraryDependencies ++= Seq(
  "org.joda" % "joda-convert" % "1.8.1" withSources() withJavadoc(),
  "joda-time" % "joda-time" % "2.9.4" withSources() withJavadoc(),
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3" withSources() withJavadoc(),
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3" withSources() withJavadoc(),
  "org.rogach" % "scallop_2.11" % "0.9.6" withSources() withJavadoc(),
  "log4j" % "log4j" % "1.2.17" withSources() withJavadoc(),

  "org.scalaj" %% "scalaj-http" % "2.3.0" withSources() withJavadoc(),
  "org.scalaz" %% "scalaz-core" % "7.1.0" withSources() withJavadoc(),
  "org.scalaz" % "scalaz-concurrent_2.11" % "7.1.0" withSources() withJavadoc(),
  
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc(),
  "org.specs2" %% "specs2-core" % "2.4.15" % "test" withSources() withJavadoc(),
  "org.specs2" %% "specs2-scalacheck" % "2.4.15" % "test" withSources() withJavadoc(),

  "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.7.2" withSources() withJavadoc(),

  "org.apache.tika" % "tika-core" % "1.13" withSources() withJavadoc()
)

resolvers ++= Seq(
  "mvnrepository" at "https://repository.cloudera.com/artifactory/cloudera-repos/",
  "Maven Central" at "https://repo1.maven.org/maven2/",
  "Sonatype OSS Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
)

assemblyMergeStrategy in assembly := {
  case el if el.contains("fasterxml.jackson.core") => MergeStrategy.first
  case el if el.contains("guava") => MergeStrategy.first

  case x if Assembly.isConfigFile(x) => MergeStrategy.concat
  case PathList(ps@_*) if Assembly.isReadme(ps.last) || Assembly.isLicenseFile(ps.last) => MergeStrategy.rename
  case PathList("META-INF", xs@_*) => (xs.map(_.toLowerCase) match {
    case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) => MergeStrategy.discard
    case ps@(x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") => MergeStrategy.discard
    case "plexus" :: xs => MergeStrategy.discard
    case "services" :: xs => MergeStrategy.filterDistinctLines
    case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) => MergeStrategy.filterDistinctLines
    case _ => MergeStrategy.first // Changed deduplicate to first
  })
  case PathList(_*) => MergeStrategy.first
}

retrieveManaged := true

pomIncludeRepository := { _ => false }

scalariformSettings
