import sbtcrossproject.CrossProject

val Version = new {
  val CaseInsensitive = "1.5.0"
  val Cats = "2.13.0"
  val CatsEffect = "3.6.0"
  val CatsParse = "1.1.0"
  val Circe = "0.14.15"
  val Data = "0.0.3"
  val EnumerationExt = "0.5.0"
  val Fs2 = "3.12.0"
  val Http4s = "1.0.0-M45"
  val Iron = "3.2.0"
  val Java = "17"
  val Kittens = "3.5.0"
  val Log4Cats = "2.7.1"
  val Mouse = "1.3.2"
  val Scala3 = "3.8.0"
  val ScalaJavaTime = "2.6.0"
  val Slf4j = "2.0.17"
  val Validation = "0.0.0+9-923780f2-SNAPSHOT"
  val Undefined = "0.0.3"
  val Zio = "2.1.24"
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

/** Base data class definitions */
lazy val core = module(identifier = Some("core"))
  .settings(
    Compile / sourceGenerators += Def.task {
      val sumInstances = (Compile / sourceManaged).value / "ConvertInstances.scala"
      IO.write(sumInstances, ConvertSourceGenerators.sumInstances(organization.value + ".otter"))
      Seq(sumInstances)
    }.taskValue,
    libraryDependencies ++=
      "io.taig" %%% "data-core" % Version.Data ::
        "io.taig" %%% "enumeration-ext-core" % Version.EnumerationExt ::
        "io.taig" %%% "undefined" % Version.Undefined ::
        "io.taig" %%% "validation-core" % Version.Validation ::
        "org.typelevel" %%% "cats-core" % Version.Cats ::
        "org.typelevel" %%% "cats-parse" % Version.CatsParse ::
        "org.typelevel" %%% "kittens" % Version.Kittens ::
        "dev.zio" %%% "zio-test-sbt" % Version.Zio % "test" ::
        Nil
  )

/** Component extensions for org.typelevel / case-insensitive */
lazy val coreCaseInsensitive = module(identifier = Some("core-case-insensitive"))
  .settings(
    libraryDependencies ++=
      "io.taig" %%% "validation-cistring" % Version.Validation ::
        "org.typelevel" %%% "case-insensitive" % Version.CaseInsensitive ::
        Nil
  )
  .dependsOn(core)

/** Component extensions for io.github.iltotore / iron */
lazy val coreIron = module(identifier = Some("core-iron"))
  .settings(
    libraryDependencies ++=
      "io.taig" %%% "validation-iron" % Version.Validation ::
        Nil
  )
  .dependsOn(core % "compile->compile;test->test")

/** Component extensions for java.time */
lazy val coreJavaTime = module(identifier = Some("core-java-time"))
  .settings(
    libraryDependencies ++=
      "io.github.cquiroz" %%% "scala-java-time" % Version.ScalaJavaTime % "test" ::
        Nil
  )
  .dependsOn(core)

/** JSON data class definitions */
lazy val coreJson = module(identifier = Some("core-json"))
  .dependsOn(core % "compile->compile;test->test")

/** TypeScript data class definitions */
lazy val coreTypescript = module(identifier = Some("core-typescript"))
  .dependsOn(core % "compile->compile;test->test")

/** Effect integration */
lazy val coreTypescriptEffect = module(identifier = Some("core-typescript-effect"))
  .dependsOn(coreTypescript % "compile->compile;test->test")

/** zod integration */
lazy val coreTypescriptZod = module(identifier = Some("core-typescript-zod"))
  .dependsOn(coreTypescript % "compile->compile;test->test")

lazy val coreJsonTypescript = module(identifier = Some("core-json-typescript"))
  .dependsOn(coreJson % "compile->compile;test->test", coreTypescript)

/** Effect codegen for JSON */
lazy val coreJsonTypescriptEffect = module(identifier = Some("core-json-typescript-effect"))
  .dependsOn(coreJson % "compile->compile;test->test", coreTypescriptEffect, coreJsonTypescript)

/** zod codegen for JSON */
lazy val coreJsonTypescriptZod = module(identifier = Some("core-json-typescript-zod"))
  .dependsOn(coreJson % "compile->compile;test->test", coreTypescriptZod, coreJsonTypescript)

/** JSON codecs for io.circe / circe */
lazy val coreJsonCirce = module(identifier = Some("core-json-circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %%% "circe-core" % Version.Circe ::
        "io.taig" %%% "data-circe" % Version.Data ::
        Nil
  )
  .dependsOn(coreJson % "compile->compile;test->test")

// lazy val http = module(identifier = Some("http"))
//   .settings(
//     libraryDependencies ++=
//       "org.typelevel" %%% "case-insensitive" % Version.CaseInsensitive ::
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

/** Opinionated dsl presets */
lazy val dsl = module(identifier = Some("dsl"))
  .dependsOn(coreCaseInsensitive, coreIron, coreJavaTime, coreJson)

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
//       "io.github.iltotore" %% "iron" % Version.Iron ::
//         Nil
//   )

// lazy val sampleApi = module(identifier = Some("sample-api"), jvmOnly = true)
//   .settings(noPublishSettings)
//   .dependsOn(dsl, sample)

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
