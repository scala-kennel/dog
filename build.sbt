import build._

lazy val jvmProjects = Seq[ProjectReference](
  coreJVM, genJVM, propsJVM, dogJVM
)

lazy val jsProjects = Seq[ProjectReference](
  coreJS, genJS, propsJS, dogJS
)

lazy val coreJS = core.js
lazy val coreJVM = core.jvm
lazy val coreRoot = project.aggregate(coreJS, coreJVM)

lazy val genJS = gen.js
lazy val genJVM = gen.jvm
lazy val genRoot = project.aggregate(genJS, genJVM)

lazy val propsJS = props.js
lazy val propsJVM = props.jvm
lazy val propsRoot = project.aggregate(propsJS, propsJVM)

lazy val dogJS = dog.js
lazy val dogJVM = dog.jvm
lazy val dogRoot = project.aggregate(dogJS, dogJVM)

val root = Project("root", file(".")).settings(
  Common.commonSettings
).settings(
  name := allName,
  packagedArtifacts := Map.empty
).aggregate(
  jvmProjects ++ jsProjects : _*
)

lazy val rootJS = project.aggregate(jsProjects: _*)
lazy val rootJVM = project.aggregate(jvmProjects: _*)
