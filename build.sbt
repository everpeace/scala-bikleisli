name:="BiKleisli on Scala"

version:="0.0.1"

scalaVersion:="2.9.1"

organization:="everpeace.org"

//scalaz
resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.3" withSources()

//compile options
scalacOptions ++= Seq("-unchecked", "-deprecation")