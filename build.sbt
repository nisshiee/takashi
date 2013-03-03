import AssemblyKeys._

name := "takashi-kun"

organization := "org.nisshiee"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.10.0"

libraryDependencies := Seq(
   "org.scalaz" %% "scalaz-core" % "7.0.0-M8"
  ,"com.github.tototoshi" %% "scala-csv" % "0.7.0"
  ,"org.specs2" %% "specs2" % "1.12.3" % "test"
  ,"org.mockito" % "mockito-all" % "1.9.0" % "test"
  ,"junit" % "junit" % "4.10" % "test"
  ,"org.pegdown" % "pegdown" % "1.1.0" % "test"
)

testOptions in (Test, test) += Tests.Argument("console", "html", "junitxml")

initialCommands := """
import scalaz._
import Scalaz._
import org.nisshiee.takashi._
"""

scalacOptions in Compile ++= Seq(
   "-language:postfixOps"
  ,"-Xfatal-warnings"
)


// ========== for scaladoc ==========

scalacOptions in (Compile, doc) <++= baseDirectory.map {
  bd => Seq("-sourcepath", bd.getAbsolutePath,
            "-doc-source-url", "https://github.com/nisshiee/takashi-kun/blob/master€{FILE_PATH}.scala")
}


// ========== for sbt-assembly ==========

seq(assemblySettings: _*)

jarName in assembly <<= (name, version) { (name, version) => name + "-" + version + ".jar" }

// test in assembly := {}

mainClass in assembly := Some("App")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case "application.conf" => MergeStrategy.concat
    case x => old(x)
  }
}
