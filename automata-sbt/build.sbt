scalaVersion := "2.11.8"

scalacOptions ++= Seq("-optimize", "-feature", "-unchecked", "-deprecation")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"

// ソースコードの在処を非標準の場所に設定

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"
