import sbtcrossproject.CrossProject

val Version = new {
  val CaseInsensitive = "1.4.0"
  val Cats = "2.12.0"
  val CatsEffect = "3.5.3"
  val CatsParse = "1.0.0"
  val Circe = "0.14.9"
  val Ducktape = "0.2.4"
  val EnumerationExt = "0.2.0"
  val Fs2 = "3.10.2"
  val Http4s = "1.0.0-M41"
  val Java = "17"
  val JNanoId = "2.0.0"
  val Log4Cats = "2.7.0"
  val Mouse = "1.3.2"
  val Munit = "1.0.1"
  val MunitCatsEffect = "1.0.7"
  val Scala3 = "3.3.3"
  val ScalaJavaTime = "2.6.0"
  val Slf4j = "2.0.12"
}

def module(identifier: Option[String], jvmOnly: Boolean = false): CrossProject = {
  val platforms = List(JVMPlatform) ++ (if (jvmOnly) Nil else List(JSPlatform))
  CrossProject(identifier.getOrElse("root"), file(identifier.fold(".")("modules/" + _)))(platforms: _*)
    .crossType(CrossType.Pure)
    .withoutSuffixFor(JVMPlatform)
    .build()
    .settings(
      Compile / console / scalacOptions -= "-Wunused:all",
      Compile / scalacOptions ++= "-source:future" :: "-rewrite" :: "-new-syntax" :: "-Wunused:all" :: Nil,
      name := "otter" + identifier.fold("")("-" + _)
    )
}

inThisBuild(
  Def.settings(
    developers := List(Developer("taig", "Niklas Klein", "mail@taig.io", url("https://taig.io/"))),
    dynverVTagPrefix := false,
    homepage := Some(url("https://github.com/taig/otter/")),
    licenses := List("MIT" -> url("https://raw.githubusercontent.com/taig/otter/main/LICENSE")),
    organization := "io.taig",
    scalaVersion := Version.Scala3,
    versionScheme := Some("early-semver")
  )
)

addCommandAlias("start", s"${sampleApp.jvm.id}/reStart")
addCommandAlias("stop", s"${sampleApp.jvm.id}/reStop")

lazy val root = module(identifier = None, jvmOnly = true)
  .enablePlugins(BlowoutYamlPlugin)
  .settings(noPublishSettings)
  .settings(
    blowoutGenerators ++= {
      val github = file(".github")
      val workflows = github / "workflows"

      BlowoutYamlGenerator.lzy(workflows / "main.yml", GithubActionsGenerator.main(Version.Java)) ::
        BlowoutYamlGenerator.lzy(workflows / "pull-request.yml", GithubActionsGenerator.pullRequest(Version.Java)) ::
        Nil
    }
  )
  .aggregate(
    core,
    javaTime,
    jsonCirce,
    openapiCirce,
    http,
    httpHttp4s,
    httpJsonCirce,
    openapi,
    server,
    serverHttp4s,
    httpCsv,
    sample,
    sampleApi,
    sampleApp
  )

lazy val core = module(identifier = Some("core"))
  .settings(
    Compile / sourceGenerators += Def.task {
      val sumInstances = (Compile / sourceManaged).value / "ConvertInstances.scala"
      IO.write(sumInstances, ConvertSourceGenerators.sumInstances(organization.value + ".otter"))
      Seq(sumInstances)
    }.taskValue,
    libraryDependencies ++=
      "io.taig" %%% "enumeration-ext-core" % Version.EnumerationExt ::
        "org.typelevel" %%% "cats-core" % Version.Cats ::
        "org.typelevel" %%% "cats-parse" % Version.CatsParse ::
        "org.scalameta" %%% "munit" % Version.Munit % "test" ::
        Nil
  )

lazy val javaTime = module(identifier = Some("java-time"))
  .settings(
    libraryDependencies ++=
      "io.github.cquiroz" %%% "scala-java-time" % Version.ScalaJavaTime % "test" ::
        Nil
  )
  .dependsOn(core)

lazy val jsonCirce = module(identifier = Some("json-circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % Version.Circe ::
        Nil
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val openapiCirce = module(identifier = Some("openapi-circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % Version.Circe ::
        Nil
  )
  .dependsOn(openapi % "compile->compile;test->test")

// lazy val typescript = module(identifier = Some("typescript"))
//   .dependsOn(core % "compile->compile;test->test")

lazy val http = module(identifier = Some("http"))
  .settings(
    libraryDependencies ++=
      "co.fs2" %%% "fs2-core" % Version.Fs2 ::
        "org.typelevel" %%% "case-insensitive" % Version.CaseInsensitive ::
        "org.typelevel" %%% "munit-cats-effect-3" % Version.MunitCatsEffect % "test" ::
        Nil
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val httpHttp4s = module(identifier = Some("http-http4s"))
  .settings(
    libraryDependencies ++=
      "org.http4s" %%% "http4s-server" % Version.Http4s ::
        Nil
  )
  .dependsOn(http % "compile->compile;test->test")

lazy val openapi = module(identifier = Some("openapi"))
  .dependsOn(http % "compile->compile;test->test")

// lazy val httpOpenapi = module(identifier = Some("http-openapi"))
//   .dependsOn(http % "compile->compile;test->test", openapi % "compile->compile;test->test")

// TODO waiting for circe 0.15 with scala.js jawn support
lazy val httpJsonCirce = module(identifier = Some("http-json-circe"), jvmOnly = true)
  .settings(
    libraryDependencies ++=
      "io.circe" %% "circe-parser" % Version.Circe ::
        Nil
  )
  .dependsOn(jsonCirce % "compile->compile;test->test", http % "compile->compile;test->test")

lazy val httpCsv = module(identifier = Some("http-csv"))
  .settings(
    libraryDependencies ++=
      Nil
  )
  .dependsOn(http % "compile->compile;test->test")

lazy val server = module(identifier = Some("server"))
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-effect" % Version.CatsEffect ::
        Nil
  )
  .dependsOn(http % "compile->compile;test->test")

lazy val serverHttp4s = module(identifier = Some("server-http4s"))
  .settings(
    libraryDependencies ++=
      "org.http4s" %%% "http4s-server" % Version.Http4s ::
        "org.typelevel" %%% "cats-effect" % Version.CatsEffect ::
        Nil
  )
  .dependsOn(server % "compile->compile;test->test", httpHttp4s % "compile->compile;test->test")

lazy val client = module(identifier = Some("client"))
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-effect" % Version.CatsEffect ::
        Nil
  )
  .dependsOn(http % "compile->compile;test->test")

// lazy val munit = module(identifier = Some("munit"))
//   .settings(
//     libraryDependencies ++=
//       "org.scalameta" %%% "munit" % Version.Munit ::
//         "org.typelevel" %%% "munit-cats-effect-3" % Version.MunitCatsEffect ::
//         Nil
//   )
//   .dependsOn(http)

lazy val sample = module(identifier = Some("sample"), jvmOnly = true)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++=
      "io.circe" %% "circe-parser" % Version.Circe ::
        "org.typelevel" %% "case-insensitive" % Version.CaseInsensitive ::
        Nil
  )

lazy val sampleApi = module(identifier = Some("sample-api"), jvmOnly = true)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++=
      Nil
  )
  .dependsOn(httpJsonCirce, httpHttp4s, openapi)

lazy val sampleApp = module(identifier = Some("sample-app"), jvmOnly = true)
  .settings(noPublishSettings)
  .settings(
    Compile / run / fork := true,
    libraryDependencies ++=
      "io.github.arainko" %% "ducktape" % Version.Ducktape ::
        "org.http4s" %% "http4s-ember-server" % Version.Http4s ::
        "org.typelevel" %% "log4cats-noop" % Version.Log4Cats ::
        "org.typelevel" %% "mouse" % Version.Mouse ::
        Nil
  )
  .dependsOn(serverHttp4s, sample, sampleApi)
