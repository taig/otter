import sbtcrossproject.CrossProject

val Version = new {
  val CaseInsensitive = "1.3.0"
  val Cats = "2.9.0"
  val CatsEffect = "3.4.9"
  val Circe = "0.14.5"
  val Fs2 = "3.6.1"
  val Http4s = "1.0.0-M39"
  val Java = "17"
  val Munit = "0.7.29"
  val MunitCatsEffect = "1.0.7"
  val Scala3 = "3.3.0"
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
      Compile / scalacOptions ++= "-source:future" :: "-rewrite" :: "-new-syntax" :: Nil,
      name := "openapi" + identifier.fold("")("-" + _)
    )
}

inThisBuild(
  Def.settings(
    developers := List(Developer("taig", "Niklas Klein", "mail@taig.io", url("https://taig.io/"))),
    dynverVTagPrefix := false,
    homepage := Some(url("https://github.com/taig/openapi/")),
    licenses := List("MIT" -> url("https://raw.githubusercontent.com/taig/openapi/main/LICENSE")),
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
  .aggregate(core, validation, schema, http, authentication, csv, dsl, circe, http4s)

lazy val core = module(identifier = Some("core"))
  .settings(
    libraryDependencies ++=
      "org.typelevel" %%% "cats-core" % Version.Cats ::
        "org.scalameta" %%% "munit" % Version.Munit % "test" ::
        "org.scalameta" %%% "munit-scalacheck" % Version.Munit % "test" ::
        Nil
  )
  .jsSettings(
    libraryDependencies ++=
      "io.github.cquiroz" %%% "scala-java-time" % Version.ScalaJavaTime % "test" ::
        Nil
  )

lazy val validation = module(identifier = Some("validation"))
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
      val sumInstances = (Compile / sourceManaged).value / "SumInstances.scala"
      IO.write(sumInstances, SchemaSourceGenerators.sumInstances(organization.value + ".openapi.schema"))
      Seq(sumInstances)
    }.taskValue
  )
  .dependsOn(core % "compile->compile;test->test", validation)

lazy val circe = module(identifier = Some("circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %%% "circe-parser" % Version.Circe ::
        Nil
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val http = module(identifier = Some("http"))
  .settings(
    libraryDependencies ++=
      "co.fs2" %%% "fs2-core" % Version.Fs2 ::
        "org.typelevel" %%% "case-insensitive" % Version.CaseInsensitive ::
        "org.typelevel" %%% "cats-effect" % Version.CatsEffect ::
        "org.typelevel" %%% "munit-cats-effect-3" % Version.MunitCatsEffect % "test" ::
        Nil
  )
  .dependsOn(schema % "compile->compile;test->test", circe)

lazy val authentication = module(identifier = Some("authentication"))
  .dependsOn(http % "compile->compile;test->test")

lazy val csv = module(identifier = Some("csv"))
  .dependsOn(schema % "compile->compile;test->test")

lazy val dsl = module(identifier = Some("dsl"))
  .dependsOn(http % "compile->compile;test->test")

lazy val http4s = module(identifier = Some("http4s"), jvmOnly = true)
  .settings(
    libraryDependencies ++=
      "org.http4s" %% "http4s-circe" % Version.Http4s ::
        Nil
  )
  .dependsOn(http % "compile->compile;test->test")

lazy val sample = module(identifier = Some("sample"), jvmOnly = true)
  .settings(
    libraryDependencies ++=
      "org.http4s" %% "http4s-ember-server" % Version.Http4s ::
        "org.slf4j" % "slf4j-simple" % Version.Slf4j ::
        Nil
  )
  .dependsOn(http4s % "compile->compile;test->test", dsl)
