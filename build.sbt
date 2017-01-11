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

lazy val jFlex = SettingKey[File]("JFlex")

lazy val exp = (project in file("exp-sbt"))
  .settings(commonSettings: _*)
  .settings(
    name := "exp-sbt"
  )

lazy val lex = (project in file("lex-sbt"))
  .settings(commonSettings: _*)
  .settings(
    name := "lex-sbt",
    name in jFlex := "Sample.flex",
    sourceDirectory in jFlex := baseDirectory.value / "flex",
    TaskKey[Unit]("jflex-generate") := {
      val logger = streams.value.log
      val process = sbt.Process(
        "java" ::
          "-jar" ::
          "lib/jflex-scala.jar" ::
          "--scala" ::
          "-d" ::
          (scalaSource in Compile).value.toString ::
          ((sourceDirectory in jFlex).value / (name in jFlex).value).toString :: Nil
      )
      val out = (process!!)
      if (out != null) logger.info(out) else logger.error("unexpected error has occured")
      ()
    }
  )

lazy val automata = (project in file("automata-sbt"))
  .settings(commonSettings: _*)
  .settings(
    name := "automata-sbt"
  )

lazy val parsing = (project in file("parsing-sbt"))
  .settings(commonSettings: _*)
  .settings(
    name := "parsing-sbt"
  )

lazy val typing = (project in file("typing-sbt"))
  .settings(commonSettings: _*)
  .settings(
    name := "typing-sbt"
  )

lazy val eval = (project in file("eval-sbt")).
  settings(commonSettings: _*).
  settings(
    name := "eval-sbt",
    assemblyJarName in assembly := "eval.jar"
  )


lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "compiler construction"
  )
  .aggregate(
    exp,
    lex,
    automata,
    parsing,
    typing,
    eval
  )
