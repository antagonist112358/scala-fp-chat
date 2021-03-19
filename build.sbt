lazy val moduleDependencies = Seq(
  // Scala
  "co.fs2"        %% "fs2-io"         % "2.5.3",
  "com.comcast"   %% "ip4s-core"      % "2.0.0-RC1",
  "org.scodec"    %% "scodec-stream"  % "2.0-109-06b4e33",
  "com.monovore"  %% "decline"        % "1.3.0",
  "com.lihaoyi"   %% "fastparse"      % "2.3.1",
  // Java
  "org.slf4j" % "slf4j-simple" % "1.7.25"
)

lazy val clientDependencies = Seq(
  "org.jline" % "jline" % "3.12.1"
)

lazy val moduleSettings = Seq(
  fork in run := true,
  fork in test := true,
  outputStrategy := Some(StdoutOutput),
  connectInput in run := true,
  scalafmtOnCompile := true,
  libraryDependencies ++= moduleDependencies,
  scalacOptions in Compile ++= List(
      "-Yresolve-term-conflict:package",
      // "-Ymacro-annotations",
      "-language:higherKinds",
      "-Xfatal-warnings",
      // These warnings (intentionally) become errors
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Ywarn-dead-code"
  ),
  scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits", "-no-link-warnings"),
  scalacOptions in Test ++= List("-Yrangepos"),
  organization := "net.mentalarray.chat",
  scalaVersion := "2.13.4",
)

lazy val compilerPlugins = Seq(
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val `fpchat-api` = (project in file("modules/fpchat-api"))
  .settings(moduleSettings)
  .settings(compilerPlugins)

lazy val `fpchat-server` = (project in file("modules/fpchat-server"))
  .dependsOn(`fpchat-api`)
  .settings(moduleSettings)
  .settings(compilerPlugins)
  .enablePlugins(JavaAppPackaging, UniversalPlugin)

lazy val `fpchat-client` = (project in file("modules/fpchat-client"))
  .settings(moduleSettings)
  .dependsOn(`fpchat-api`)
  .settings(libraryDependencies ++= clientDependencies)
  .settings(compilerPlugins)
  .enablePlugins(JavaAppPackaging, UniversalPlugin)