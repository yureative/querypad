import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "QueryPad"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "com.jolbox" % "bonecp" % "0.8.0-rc1",
    "postgresql" % "postgresql" % "9.1-901.jdbc4"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
