import sbtcrossproject.CrossProject

val Version = new {
  val CaseInsensitive = "1.4.0"
  val Cats = "2.10.0"
  val CatsEffect = "3.5.3"
  val Circe = "0.14.6"
  val EnumerationExt = "0.0.3"
  val Http4s = "1.0.0-M40"
  val Java = "17"
  val JNanoId = "2.0.0"
  val Log4Cats = "2.6.0"
  val Mouse = "1.2.3"
  val Munit = "0.7.29"
  val MunitCatsEffect = "1.0.7"
  val Scala3 = "3.3.3"
  val ScalaJavaTime = "2.5.0"
  val Slf4j = "2.0.12"
}

def module(identifier: Option[String], jvmOnly: Boolean = false): CrossProject = {
  val platforms = List(JVMPlatform) ++ (if (jvmOnly) Nil else List(JSPlatform))
  CrossProject(identifier.getOrElse("root"), file(identifier.fold(".")("modules/" + _)))(platforms: _*)
    .crossType(CrossType.Pure)
    .withoutSuffixFor(JVMPlatform)
    .build()
    .settings(
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

addCommandAlias("start", s"${sample.jvm.id}/reStart")
addCommandAlias("stop", s"${sample.jvm.id}/reStop")

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
    circe,
    openapi,
    typescript,
    openapiCirce,
    http,
    httpOpenapi,
    httpCirce,
    server,
    csv,
    dsl,
    http4s,
    munit,
    sample
  )

lazy val core = module(identifier = Some("core"))
  .settings(
    Compile / sourceGenerators += Def.task {
      val sumInstances = (Compile / sourceManaged).value / "CoproductInstances.scala"
      IO.write(sumInstances, SchemaSourceGenerators.sumInstances(organization.value + ".otter"))
      Seq(sumInstances)
    }.taskValue,
    libraryDependencies ++=
      "io.taig" %%% "enumeration-ext-core" % Version.EnumerationExt ::
        "org.typelevel" %%% "cats-core" % Version.Cats ::
        "org.typelevel" %%% "case-insensitive" % Version.CaseInsensitive ::
        "io.github.cquiroz" %%% "scala-java-time" % Version.ScalaJavaTime % "test" ::
        "org.scalameta" %%% "munit" % Version.Munit % "test" ::
        "org.scalameta" %%% "munit-scalacheck" % Version.Munit % "test" ::
        Nil
  )
  .jsSettings(
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % "test")
      .cross(CrossVersion.for3Use2_13)
  )

lazy val circe = module(identifier = Some("circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % Version.Circe ::
        Nil
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val openapi = module(identifier = Some("openapi"))
  .dependsOn(core % "compile->compile;test->test")

lazy val openapiCirce = module(identifier = Some("openapi-circe"))
  .dependsOn(openapi % "compile->compile;test->test", circe % "compile->compile;test->test")

lazy val typescript = module(identifier = Some("typescript"))
  .dependsOn(core % "compile->compile;test->test")

lazy val http = module(identifier = Some("http"))
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "munit-cats-effect-3" % Version.MunitCatsEffect % "test" ::
        Nil
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val httpOpenapi = module(identifier = Some("http-openapi"))
  .dependsOn(http % "compile->compile;test->test", openapi % "compile->compile;test->test")

// TODO waiting for circe 0.15 with scala.js jawn support
lazy val httpCirce = module(identifier = Some("http-circe"), jvmOnly = true)
  .settings(
    libraryDependencies ++=
      "io.circe" %% "circe-parser" % Version.Circe ::
        Nil
  )
  .dependsOn(circe % "compile->compile;test->test", http % "compile->compile;test->test")

lazy val server = module(identifier = Some("server"))
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-effect" % Version.CatsEffect ::
        Nil
  )
  .dependsOn(http % "compile->compile;test->test")

lazy val csv = module(identifier = Some("csv"))
  .dependsOn(core % "compile->compile;test->test")

lazy val dsl = module(identifier = Some("dsl"), jvmOnly = true)
  .dependsOn(httpCirce % "compile->compile;test->test", server % "compile->compile;test->test")

lazy val http4s = module(identifier = Some("http4s"), jvmOnly = true)
  .settings(
    libraryDependencies ++=
      "org.http4s" %%% "http4s-server" % Version.Http4s ::
        Nil
  )
  .dependsOn(server % "compile->compile;test->test")

lazy val munit = module(identifier = Some("munit"))
  .settings(
    libraryDependencies ++=
      "org.scalameta" %%% "munit" % Version.Munit ::
        "org.typelevel" %%% "munit-cats-effect-3" % Version.MunitCatsEffect ::
        Nil
  )
  .dependsOn(http)

lazy val sample = module(identifier = Some("sample"), jvmOnly = true)
  .enablePlugins(BuildInfoPlugin)
  .settings(noPublishSettings)
  .settings(
    buildInfoKeys := Seq(version),
    buildInfoObject := "Build",
    buildInfoPackage := organization.value + ".otter.sample",
    libraryDependencies ++=
      "com.aventrix.jnanoid" % "jnanoid" % Version.JNanoId ::
        "org.http4s" %% "http4s-ember-server" % Version.Http4s ::
        "org.http4s" %% "http4s-dsl" % Version.Http4s ::
        "org.slf4j" % "slf4j-simple" % Version.Slf4j ::
        "org.typelevel" %% "log4cats-slf4j" % Version.Log4Cats ::
        "org.typelevel" %% "mouse" % Version.Mouse ::
        Nil
  )
  .dependsOn(http4s, dsl, httpOpenapi, openapiCirce, typescript, munit % "compile->test")
