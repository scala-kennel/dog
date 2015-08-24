import sbt._
import Keys._
import Common._
import Dependencies._

object build extends Build {

  private[this] val coreName = "dog-core"
  private[this] val allName = "dog-all"
  private[this] val dogName = "dog"

  private[this] def module(id: String) =
    Project(id, file(id)).settings(commonSettings)

  lazy val core = module("core").settings(
    name := coreName,
    libraryDependencies ++= Seq(
      scalaz,
      scalatest % "test"
    )
  )

  lazy val dog = module(dogName).settings(
    name := dogName,
    libraryDependencies += testInterface
  ).dependsOn(core)

  val root = Project("root", file(".")).settings(
    commonSettings ++
    xerial.sbt.Sonatype.sonatypeRootSettings ++ (
      core ::
      Nil
    ).map(libraryDependencies <++= libraryDependencies in _)
  ).settings(
    name := allName,
    artifacts := Nil,
    packagedArtifacts := Map.empty,
    artifacts <++= Classpaths.artifactDefs(Seq(packageDoc in Compile)),
    packagedArtifacts <++= Classpaths.packaged(Seq(packageDoc in Compile))
  ).aggregate(
    core, dog
  )
}
