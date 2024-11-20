val commonSettings = Seq(
    scalaVersion := "3.5.0",
    scalacOptions ++= List("-deprecation", "-feature", "-language:fewerBraces", "-Xfatal-warnings")
)

/// Dependencies

val webappLibRepo = uri("https://gitlab.epfl.ch/cs214/ul2024/webapp-lib.git#v0.8.0")

lazy val client: ProjectReference = ProjectRef(webappLibRepo, "webappLibJS")
lazy val server: ProjectReference = ProjectRef(webappLibRepo, "webappLibJVM")

/// Commands

lazy val copyJsTask = TaskKey[Unit]("copyJsTask", "Copy javascript files to server target directory")

/// Aggregate project

lazy val app = (crossProject(JVMPlatform, JSPlatform) in file("./apps"))
  .settings(
    commonSettings
  ).settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "ujson" % "3.3.1",
    )
  ).jsSettings(
    scalaJSUseMainModuleInitializer := true,
    test / aggregate := false,
    Test / test := {},
    Test / testOnly := {},
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.lihaoyi" %%% "scalatags" % "0.12.0",
    )
  ).jvmSettings(
    run / fork := true,
    Global / cancelable := true,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "toolkit-test" % "0.2.1" % Test,
    )
  )

lazy val appJS = app.js.dependsOn(client)
lazy val appJVM = app.jvm.dependsOn(server % "compile->compile;test->test")

/// Aggregate project

lazy val webappApp = (project in file("."))
  .settings(
    commonSettings
  ).aggregate(
    appJS, appJVM,
  ).settings(
    name := "webappApp",
    copyJsTask := {
      println("[info] Copying generated main.js to server's static files directory...")
      val inDir = baseDirectory.value / "apps/js/target/scala-3.5.0/app-fastopt/"
      val outDir = baseDirectory.value / "apps/jvm/src/main/resources/www/static/"
      Seq("main.js", "main.js.map") map { p => (inDir / p, outDir / p) } foreach { f => IO.copyFile(f._1, f._2) }
    },
    Test / test := Def.sequential(
      (Test / testOnly).toTask("")
    ).value,
    Compile / run := Def.sequential(
      Compile / compile,
      (appJS / Compile / fastLinkJS).toTask,
      copyJsTask,
      (appJVM / Compile / run).toTask(""),
    ).value,
  )
