import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "circlelocus"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "org.anormcypher" %% "anormcypher" % "0.2.1"
    )

    val main = PlayProject(appName, appVersion, appDependencies).settings(
      // Add your own project settings here
      resolvers += "anormcypher" at "http://repo.anormcypher.org/"
    )

}
