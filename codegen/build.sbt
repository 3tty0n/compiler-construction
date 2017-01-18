scalaVersion := "2.11.8"
scalacOptions ++= Seq("-optimize", "-feature", "-unchecked", "-deprecation")
javaOptions in run ++= Seq("-Xmx2G", "-verbose:gc")
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
initialCommands in console :=
  """
    |import nonscala._
    |import Abssyn._
    |import Oper._
    |import CodegenBase._
    |import ILExec._
  """.stripMargin
