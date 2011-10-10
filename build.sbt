name:="BiKleisli on Scala"

version:="0.0.1"

scalaVersion:="2.9.1"

organization:="everpeace.org"

//scalaz
resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.2" withSources()

//compile options
scalacOptions ++= Seq("-unchecked", "-deprecation")

//scala xray (sxr)
addCompilerPlugin("org.scala-tools.sxr" % "sxr_2.9.0" % "0.2.7")

scalacOptions <+= scalaSource in Compile map { "-P:sxr:base-directory:" + _.getAbsolutePath }

scalacOptions <+= baseDirectory map { base =>
  val linkFile = base / "sxr.links"
  "-P:sxr:link-file:" + linkFile.getAbsolutePath
}
