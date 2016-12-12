lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  version := "0.1.0",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.scalaz" %% "scalaz-core" % "7.2.8"
  ),
  scalacOptions ++= Seq(
    "-optimize",
    "-feature",
    "-unchecked",
    "-deprecation"
  ),
  javaOptions in run ++= Seq(
    "-Xmx2G",
    "-verbose:gc"
  )
)

lazy val exp = (project in file("exp-sbt"))
  .settings(commonSettings: _*)
  .settings(
    name := "exp-sbt"
  )

lazy val lex = (project in file("lex-sbt"))
  .settings(commonSettings: _*)
  .settings(
    name := "lex-sbt"
  )

lazy val automata = (project in file("automata-sbt"))
  .settings(commonSettings: _*)
  .settings(
    name := "automata-sbt"
  )

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "compiler construction"
  )
  .aggregate(
    exp,
    lex,
    automata
  )
