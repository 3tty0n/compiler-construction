lazy val root = (project in file(".")).dependsOn(githubRepo)

lazy val githubRepo = uri("git://github.com/3tty0n/sbt-jflex-plugin#master")