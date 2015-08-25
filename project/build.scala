import sbt._
import Keys._
import Common._
import Dependencies._

object build extends Build {

  private[this] val coreName = "dog-core"
  private[this] val genName = "dog-gen"
  private[this] val propsName = "dog-props"
  private[this] val allName = "dog-all"
  private[this] val dogName = "dog"

  private[this] def module(id: String) =
    Project(id, file(id)).settings(commonSettings)

  lazy val core = module("core").settings(
    name := coreName,
    libraryDependencies ++= Seq(
      scalazConcurrent
    )
  )

  lazy val gen = module("gen").settings(
    name := genName,
    libraryDependencies ++= Seq(
      scalapropsGen
    )
  ).dependsOn(core)

  lazy val props = module("props").settings(
    name := propsName,
    libraryDependencies ++= Seq(
      scalapropsCore
    )
  ).dependsOn(core)

  lazy val dog = module(dogName).settings(
    name := dogName,
    libraryDependencies += testInterface,
    testFrameworks += new TestFramework("dog.DogFramework")
  ).dependsOn(core, gen % "test", props % "test")

  val root = Project("root", file(".")).settings(
    commonSettings ++
    xerial.sbt.Sonatype.sonatypeRootSettings ++ (
      core ::
      gen ::
      props ::
      dog ::
      Nil
    ).map(libraryDependencies <++= libraryDependencies in _)
  ).settings(
    name := allName,
    artifacts := Nil,
    packagedArtifacts := Map.empty,
    artifacts <++= Classpaths.artifactDefs(Seq(packageDoc in Compile)),
    packagedArtifacts <++= Classpaths.packaged(Seq(packageDoc in Compile))
  ).aggregate(
    core, gen, dog
  )
}
