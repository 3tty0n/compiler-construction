scalaVersion := "2.11.8"

scalacOptions ++= Seq("-optimize", "-feature", "-unchecked", "-deprecation")

javaOptions in run ++= Seq( "-Xmx2G", "-verbose:gc")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0" % "test"

// ソースコードの在処を非標準の場所に設定

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"
