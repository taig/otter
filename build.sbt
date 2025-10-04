import org.checkerframework.checker.units.qual.m
import sbtcrossproject.CrossProject

val Version = new {
  val CaseInsensitive = "1.5.0"
  val Cats = "2.13.0"
  val CatsEffect = "3.6.0"
  val CatsParse = "1.1.0"
  val Circe = "0.14.14"
  val Data = "0.0.2"
  val EnumerationExt = "0.5.0"
  val Fs2 = "3.12.0"
  val Http4s = "1.0.0-M45"
  val Java = "17"
  val JNanoId = "2.0.0"
  val Kittens = "3.5.0"
  val Log4Cats = "2.7.1"
  val Mouse = "1.3.2"
  val Munit = "1.2.0"
  val MunitCatsEffect = "1.0.7"
  val Scala3 = "3.3.6"
  val ScalaJavaTime = "2.6.0"
  val Slf4j = "2.0.17"
  val Validation = "HEAD+20250930-0843"
  val Undefined = "0.0.3"
}

def module(identifier: Option[String], jvmOnly: Boolean = false): CrossProject = {
  val platforms = List(JVMPlatform) ++ (if (jvmOnly) Nil else List(JSPlatform))
  CrossProject(identifier.getOrElse("root"), file(identifier.fold(".")("modules/" + _)))(platforms: _*)
    .crossType(CrossType.Pure)
    .withoutSuffixFor(JVMPlatform)
    .build()
    .settings(
      Compile / console / scalacOptions -= "-Wunused:all",
      Compile / scalacOptions ++= "-source:future" :: "-rewrite" :: "-new-syntax" :: "-Wunused:all" :: "-Xmax-inlines" :: "64" :: Nil,
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

// addCommandAlias("start", s"${sampleApp.jvm.id}/reStart")
// addCommandAlias("stop", s"${sampleApp.jvm.id}/reStop")

noPublishSettings

lazy val root = module(identifier = None, jvmOnly = true)
  .enablePlugins(BlowoutYamlPlugin)
  .settings(noPublishSettings)
  .settings(
    blowoutGenerators ++= {
      val github = file(".github")
      val workflows = github / "workflows"

      BlowoutYamlGenerator.lzy(workflows / "main.yml", GithubActionsGenerator.main(Version.Java)) ::
        BlowoutYamlGenerator.lzy(workflows / "pull-request.yml", GithubActionsGenerator.pullRequest(Version.Java)) ::
        BlowoutYamlGenerator.lzy(workflows / "tag.yml", GithubActionsGenerator.tag(Version.Java)) ::
        Nil
    }
  )
  .aggregate(core, coreCaseInsensitive, coreJson, coreJsonCirce)

lazy val core = module(identifier = Some("core"))
  .settings(
    Compile / sourceGenerators += Def.task {
      val sumInstances = (Compile / sourceManaged).value / "ConvertInstances.scala"
      // IO.write(sumInstances, ConvertSourceGenerators.sumInstances(organization.value + ".otter"))
      // Seq(sumInstances)
      Nil
    }.taskValue,
    libraryDependencies ++=
      "io.taig" %%% "data-core" % Version.Data ::
        "io.taig" %%% "enumeration-ext-core" % Version.EnumerationExt ::
        "io.taig" %%% "undefined" % Version.Undefined ::
        "io.taig" %%% "validation-core" % Version.Validation ::
        "org.typelevel" %%% "cats-core" % Version.Cats ::
        "org.typelevel" %%% "cats-parse" % Version.CatsParse ::
        "org.typelevel" %%% "kittens" % Version.Kittens ::
        "org.scalameta" %%% "munit" % Version.Munit % "test" ::
        Nil
  )

lazy val coreCaseInsensitive = module(identifier = Some("core-case-insensitive"))
  .settings(
    libraryDependencies ++=
      "io.taig" %%% "validation-cistring" % Version.Validation ::
        "org.typelevel" %%% "case-insensitive" % Version.CaseInsensitive ::
        Nil
  )
  .dependsOn(core)

// lazy val coreJavaTime = module(identifier = Some("core-java-time"))
//   .settings(
//     libraryDependencies ++=
//       "io.github.cquiroz" %%% "scala-java-time" % Version.ScalaJavaTime % "test" ::
//         Nil
//   )
//   .dependsOn(core)

lazy val coreJson = module(identifier = Some("core-json"))
  .dependsOn(core % "compile->compile;test->test")

// lazy val coreTypescript = module(identifier = Some("core-typescript"))
//   .dependsOn(core % "compile->compile;test->test")

// lazy val coreEffect = module(identifier = Some("core-effect"))
//   .dependsOn(core % "compile->compile;test->test")

// lazy val coreTypescriptEffect = module(identifier = Some("core-typescript-effect"))
//   .dependsOn(coreTypescript % "compile->compile;test->test", coreEffect % "compile->compile;test->test")

lazy val coreJsonCirce = module(identifier = Some("core-json-circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % Version.Circe ::
        "io.taig" %%% "data-circe" % Version.Data ::
        Nil
  )
  .dependsOn(coreJson % "compile->compile;test->test")

// lazy val coreJsonEffect = module(identifier = Some("core-json-effect"))
//   .dependsOn(coreJson % "compile->compile;test->test", coreEffect % "compile->compile;test->test")

// lazy val coreJsonTypescript = module(identifier = Some("core-json-typescript"))
//   .dependsOn(coreJson % "compile->compile;test->test", coreTypescript % "compile->compile;test->test")

// lazy val coreJsonTypescriptEffect = module(identifier = Some("core-json-typescript-effect"))
//   .dependsOn(
//     coreTypescriptEffect % "compile->compile;test->test",
//     coreJsonEffect % "compile->compile;test->test",
//     coreJsonTypescript % "compile->compile;test->test"
//   )

// lazy val http = module(identifier = Some("http"))
//   .settings(
//     libraryDependencies ++=
//       "org.typelevel" %%% "case-insensitive" % Version.CaseInsensitive ::
//         "org.typelevel" %%% "munit-cats-effect-3" % Version.MunitCatsEffect % "test" ::
//         Nil
//   )
//   .dependsOn(core % "compile->compile;test->test")

// lazy val httpJson = module(identifier = Some("http-json"))
//   .dependsOn(http % "compile->compile;test->test", coreJson % "compile->compile;test->test")

// lazy val httpJsonCirce = module(identifier = Some("http-json-circe"))
//   .settings(
//     libraryDependencies ++=
//       "io.circe" %%% "circe-jawn" % Version.Circe ::
//         Nil
//   )
//   .dependsOn(httpJson % "compile->compile;test->test", coreJsonCirce % "compile->compile;test->test")

// lazy val httpHttp4s = module(identifier = Some("http-http4s"))
//   .settings(
//     libraryDependencies ++=
//       "org.http4s" %%% "http4s-server" % Version.Http4s ::
//         Nil
//   )
//   .dependsOn(http % "compile->compile;test->test")

// lazy val httpTypescriptEffect = module(identifier = Some("http-typescript-effect"))
//   .dependsOn(http % "compile->compile;test->test", coreJsonTypescriptEffect % "compile->compile;test->test")

// lazy val munit = module(identifier = Some("munit"))
//   .settings(
//     libraryDependencies ++=
//       "org.scalameta" %%% "munit" % Version.Munit ::
//         "org.typelevel" %%% "munit-cats-effect-3" % Version.MunitCatsEffect ::
//         Nil
//   )
//   .dependsOn(http)

// lazy val sample = module(identifier = Some("sample"), jvmOnly = true)
//   .settings(noPublishSettings)
//   .settings(
//     libraryDependencies ++=
//       "io.circe" %% "circe-parser" % Version.Circe ::
//         "org.typelevel" %% "case-insensitive" % Version.CaseInsensitive ::
//         Nil
//   )

// lazy val sampleApi = module(identifier = Some("sample-api"), jvmOnly = true)
//   .settings(noPublishSettings)
//   .dependsOn(coreJson, coreCaseInsensitive, coreJavaTime, httpJson, httpHttp4s)

// lazy val sampleApp = module(identifier = Some("sample-app"), jvmOnly = true)
//   .settings(noPublishSettings)
//   .settings(
//     libraryDependencies ++=
//       "org.http4s" %% "http4s-ember-server" % Version.Http4s ::
//         "org.slf4j" % "slf4j-simple" % Version.Slf4j ::
//         "org.typelevel" %% "log4cats-noop" % Version.Log4Cats ::
//         "org.typelevel" %% "log4cats-slf4j" % Version.Log4Cats ::
//         "org.typelevel" %% "mouse" % Version.Mouse ::
//         Nil
//   )
//   .dependsOn(sampleApi, httpJsonCirce, httpTypescriptEffect, munit % "compile->test")
