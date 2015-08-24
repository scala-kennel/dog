import sbt._, Keys._

object Dependencies {

  object Version {
    val scalaz = "7.1.3"
    val scalaprops = "0.1.13"
    val testInterface = "1.0"
  }

  val scalaz = "org.scalaz" %% "scalaz-core" % Version.scalaz
  val testInterface = "org.scala-sbt" % "test-interface" % Version.testInterface
  val scalapropsGen = "com.github.scalaprops" %% "scalaprops-gen" % Version.scalaprops
  val scalapropsCore = "com.github.scalaprops" %% "scalaprops-core" % Version.scalaprops
}
