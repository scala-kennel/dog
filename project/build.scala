import sbt._
import Keys._
import Common._
import Dependencies._
import org.scalajs.sbtplugin.cross.{CrossType, CrossProject}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object build extends Build {

  private[this] val coreName = "dog-core"
  private[this] val genName = "dog-gen"
  private[this] val propsName = "dog-props"
  private[this] val allName = "dog-all"
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

  lazy val coreJS = core.js
  lazy val coreJVM = core.jvm
  lazy val coreRoot = project.aggregate(coreJS, coreJVM)

  lazy val gen = module("gen").settings(
    name := genName,
    libraryDependencies ++= Seq(
      "com.github.scalaprops" %%% "scalaprops-gen" % Version.scalaprops
    )
  ).dependsOn(core)

  lazy val genJS = gen.js
  lazy val genJVM = gen.jvm
  lazy val genRoot = project.aggregate(genJS, genJVM)

  lazy val props = module("props").settings(
    name := propsName,
    libraryDependencies ++= Seq(
      "com.github.scalaprops" %%% "scalaprops-core" % Version.scalaprops
    )
  ).dependsOn(core)

  lazy val propsJS = props.js
  lazy val propsJVM = props.jvm
  lazy val propsRoot = project.aggregate(propsJS, propsJVM)

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
      "org.scalaz" %%% "scalaz-concurrent" % Version.scalaz,
      "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
    )
  ).jsSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
    )
  )

  lazy val dogJS = dog.js
  lazy val dogJVM = dog.jvm
  lazy val dogRoot = project.aggregate(dogJS, dogJVM)

  private[this] lazy val jvmProjects = Seq[ProjectReference](
    coreJVM, genJVM, propsJVM, dogJVM
  )
  private[this] lazy val jsProjects = Seq[ProjectReference](
    coreJS, genJS, propsJS, dogJS
  )

  val root = Project("root", file(".")).settings(
    commonSettings
  ).settings(
    name := allName,
    packagedArtifacts := Map.empty
  ).aggregate(
    jvmProjects ++ jsProjects : _*
  )

  lazy val rootJS = project.aggregate(jsProjects: _*)
  lazy val rootJVM = project.aggregate(jvmProjects: _*)
}
