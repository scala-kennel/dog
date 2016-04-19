import sbt._, Keys._

object Dependencies {

  object Version {
    val scalaz = "7.2.2"
    val scalaprops = "0.3.0"
    val testInterface = "1.0"
  }

  val testInterface = "org.scala-sbt" % "test-interface" % Version.testInterface
}
