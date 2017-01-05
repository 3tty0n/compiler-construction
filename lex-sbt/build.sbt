val jflex = settingKey[File]("jflex")

scalaVersion := "2.11.8"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

name in jflex := "Sample.flex"

sourceDirectory in jflex := baseDirectory.value / "flex"

TaskKey[Unit]("generate") := {
  val logger = streams.value.log
  val process = sbt.Process(
    "java" ::
      "-jar" ::
      "lib/jflex-scala.jar" ::
      "--scala" ::
      "-d" ::
      (scalaSource in Compile).value.toString ::
      ((sourceDirectory in jflex).value / (name in jflex).value).toString :: Nil
  )
  val out = (process!!)
  if (out != null) logger.info(out) else logger.error("unexpected error has occured")
  ()
}