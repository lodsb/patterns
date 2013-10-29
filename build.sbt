name := "patterns"

organization := "org.lodsb"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-unchecked", "-deprecation") //, "-Xprint:typer")

scalacOptions <++= scalaVersion map { version =>
  val Version = """(\d+)\.(\d+)\..*"""r
  val Version(major0, minor0) = version map identity
  val (major, minor) = (major0.toInt, minor0.toInt)
  if (major < 2 || (major == 2 && minor < 10))
  	Seq("-Ydependent-method-types")
 	else Nil
}

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

unmanagedBase <<= baseDirectory { base => base / "libraries" }

resolvers += "Twitter repo" at "http://maven.twttr.com/"

//libraryDependencies ++= Seq(
//    "com.twitter" %% "util-eval" % "[6.2.4,)"
//)

//libraryDependencies += "org.lodsb" %% "reakt" % "0.1-SNAPSHOT"

//libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.3"

//libraryDependencies += "pl.project13.scala" %% "macro-method-alias" % "1.0"

