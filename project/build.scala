import sbt._
import Keys._
import Common._
import Dependencies._
import org.scalajs.sbtplugin.cross.{CrossType, CrossProject}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object build {

  private[this] val coreName = "dog-core"
  private[this] val genName = "dog-gen"
  private[this] val propsName = "dog-props"
  val allName = "dog-all"
  private[this] val dogName = "dog"

  private[this] def module(id: String) =
    CrossProject(id, file(id), CustomCrossType).settings(
      commonSettings: _*
    ).settings(
      scalaJSStage in Test := FastOptStage,
      jsEnv := NodeJSEnv().value
    )

  val modules: List[String] = (
    coreName ::
    genName ::
    propsName ::
    allName ::
    dogName ::
    Nil
  )

  // avoid move files
  // https://github.com/scala-js/scala-js/blob/v0.6.7/sbt-plugin/src/main/scala/scala/scalajs/sbtplugin/cross/CrossProject.scala#L193-L206
  object CustomCrossType extends CrossType {
    override def projectDir(crossBase: File, projectType: String) =
      crossBase / projectType

    def shared(projectBase: File, conf: String) =
      projectBase.getParentFile / "src" / conf / "scala"

    override def sharedSrcDir(projectBase: File, conf: String) =
      Some(shared(projectBase, conf))
  }

  lazy val core = module("core").settings(
    name := coreName,
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core" % Version.scalaz
    )
  )

  lazy val gen = module("gen").settings(
    name := genName,
    libraryDependencies ++= Seq(
      "com.github.scalaprops" %%% "scalaprops-gen" % Version.scalaprops
    )
  ).dependsOn(core)

  lazy val props = module("props").settings(
    name := propsName,
    libraryDependencies ++= Seq(
      "com.github.scalaprops" %%% "scalaprops-core" % Version.scalaprops
    )
  ).dependsOn(core)

  lazy val dog = module(dogName).settings(
    name := dogName,
    testFrameworks += new TestFramework("dog.DogFramework"),
    libraryDependencies ++= Seq(
      "com.github.scalaprops" %%% "scalaprops-scalazlaws" % Version.scalaprops % "test"
    )
  ).dependsOn(
    core, gen % "test", props % "test"
  ).jvmSettings(
    libraryDependencies ++= Seq(
      testInterface,
      "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
    )
  ).jsSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
    )
  )
}
