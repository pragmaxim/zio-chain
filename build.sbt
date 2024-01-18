ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

enablePlugins(JavaAppPackaging)

lazy val root = (project in file("."))
  .settings(
    name := "zio-chain"
  )

libraryDependencies ++= Seq(
  "dev.zio"           %% "zio"                      % "2.0.21",
  "dev.zio"           %% "zio-streams"              % "2.0.21",
  "org.slf4j"         % "slf4j-nop"                 % "2.0.9",
  "dev.zio"           %% "zio-test"                 % "2.0.21" % Test,
  "dev.zio"           %% "zio-test-sbt"             % "2.0.21" % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
