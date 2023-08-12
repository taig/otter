import sbtcrossproject.CrossProject

val Version = new {
  val CaseInsensitive = "1.4.0"
  val Cats = "2.9.0"
  val Circe = "0.14.5"
  val EnumerationExt = "0.0.2"
  val Http4s = "1.0.0-M40"
  val Java = "17"
  val Munit = "0.7.29"
  val MunitCatsEffect = "1.0.7"
  val Scala3 = "3.3.1-RC5"
  val ScalaJavaTime = "2.5.0"
  val Slf4j = "2.0.7"
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
  .aggregate(core, schema, http, csv, circe, dsl, openapi, http4s, sample)

lazy val core = module(identifier = Some("core"))
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-core" % Version.Cats ::
        "org.scalameta" %%% "munit" % Version.Munit % "test" ::
        "org.scalameta" %%% "munit-scalacheck" % Version.Munit % "test" ::
        Nil
  )

lazy val schema = module(identifier = Some("schema"))
  .settings(
    Compile / sourceGenerators += Def.task {
      val sumInstances = (Compile / sourceManaged).value / "CoproductInstances.scala"
      IO.write(sumInstances, SchemaSourceGenerators.sumInstances(organization.value + ".otter.schema"))
      Seq(sumInstances)
    }.taskValue,
    libraryDependencies ++=
      "io.taig" %%% "enumeration-ext-core" % Version.EnumerationExt ::
        "org.typelevel" %%% "case-insensitive" % Version.CaseInsensitive ::
        Nil
  )
  .jsSettings(
    libraryDependencies += ("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0" % "test")
      .cross(CrossVersion.for3Use2_13)
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val http = module(identifier = Some("http"))
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "munit-cats-effect-3" % Version.MunitCatsEffect % "test" ::
        Nil
  )
  .dependsOn(schema % "compile->compile;test->test")

lazy val csv = module(identifier = Some("csv"))
  .dependsOn(schema % "compile->compile;test->test")

lazy val circe = module(identifier = Some("circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %%% "circe-parser" % Version.Circe ::
        Nil
  )
  .dependsOn(http % "compile->compile;test->test")

lazy val dsl = module(identifier = Some("dsl"))
  .dependsOn(circe % "compile->compile;test->test", http % "compile->compile;test->test")

lazy val openapi = module(identifier = Some("openapi")).dependsOn(circe, http)

lazy val http4s = module(identifier = Some("http4s"))
  .settings(
    libraryDependencies ++=
      "org.http4s" %%% "http4s-circe" % Version.Http4s ::
        "org.http4s" %%% "http4s-ember-server" % Version.Http4s ::
        Nil
  )
  .dependsOn(http % "compile->compile;test->test", circe)

lazy val sample = module(identifier = Some("sample"), jvmOnly = true)
  .settings(noPublishSettings)
  .settings(
    libraryDependencies ++=
      "org.http4s" %% "http4s-ember-server" % Version.Http4s ::
        "org.http4s" %% "http4s-dsl" % Version.Http4s ::
        "org.slf4j" % "slf4j-simple" % Version.Slf4j ::
        "org.typelevel" %% "log4cats-slf4j" % "2.6.0" ::
        "org.slf4j" % "slf4j-simple" % "2.0.7" ::
        Nil
  )
  .dependsOn(openapi, http4s)
