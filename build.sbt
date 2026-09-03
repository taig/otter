import sbtcrossproject.CrossProject

/** True on a CI runner, which has an order of magnitude less memory than a development machine and links every project
  * exactly once, so nothing it holds on to for a second run is ever read.
  */
val ci: Boolean = sys.env.contains("CI")

def module(identifier: Option[String], jvmOnly: Boolean = false): CrossProject = {
  val platforms = List(JVMPlatform) ++ (if (jvmOnly) Nil else List(JSPlatform))
  val project = CrossProject(identifier.getOrElse("root"), file(identifier.fold(".")("modules/" + _)))(platforms *)
    .crossType(CrossType.Pure)
    .withoutSuffixFor(JVMPlatform)
    .build()
    .settings(
      Compile / console / scalacOptions -= "-Wunused:all",
      Compile / scalacOptions ++= "-source:future" :: "-rewrite" :: "-new-syntax" :: "-Wunused:all" ::
        "-Xmax-inlines" :: "64" :: Nil,
      name := "otter" + identifier.fold("")("-" + _)
    )

  if (jvmOnly) project else project.jsSettings(scalaJSLinkerConfig ~= { _.withBatchMode(ci) })
}

// A linker holds the IR of its entire classpath, and there are thirteen of them. Two at once is what a runner with two
// cores would otherwise attempt, and one is what it can afford.
Global / concurrentRestrictions ++= (if (ci) Tags.limitAll(1) :: Nil else Nil)

inThisBuild(
  Def.settings(
    developers := List(Developer("taig", "Niklas Klein", "mail@taig.io", uri("https://taig.io/"))),
    dynverVTagPrefix := false,
    homepage := Some(uri("https://github.com/taig/otter/")),
    licenses := List("MIT" -> uri("https://raw.githubusercontent.com/taig/otter/main/LICENSE")),
    organization := "io.taig",
    resolvers += Resolver.sonatypeCentralSnapshots,
    scalaVersion := Version.Scala3,
    versionScheme := Some("early-semver")
  )
)

lazy val root = module(identifier = None, jvmOnly = true)
  .enablePlugins(BlowoutYamlPlugin)
  .settings(noPublishSettings)
  .settings(
    blowoutGenerators ++= {
      val workflows = file(".github") / "workflows"

      BlowoutYamlGenerator.lzy(workflows / "main.yml", GithubActionsGenerator.main(Version.Java)) ::
        BlowoutYamlGenerator.lzy(workflows / "pull-request.yml", GithubActionsGenerator.pullRequest(Version.Java)) ::
        BlowoutYamlGenerator.lzy(workflows / "tag.yml", GithubActionsGenerator.tag(Version.Java)) ::
        Nil
    }
  )
  .aggregate(modules *)

/** Every module that cross builds, which is every module but the root: what there is to test, twice over. */
lazy val modules: List[CrossProject] = List(
  core,
  coreCaseInsensitive,
  coreCsv,
  coreCsvFs2Data,
  coreIron,
  coreJavaTime,
  coreJson,
  coreJsonCirce,
  coreJsonSchema,
  coreJsonTypescript,
  coreJsonTypescriptEffect,
  coreTypescript,
  coreTypescriptEffect,
  http
)

/** Format agnostic schema definitions and interpreters */
lazy val core = module(identifier = Some("core"))
  .jsSettings(
    libraryDependencies += "io.github.cquiroz" %% "locales-full-currencies-db" % Version.ScalaJavaLocales % Test
  )
  .settings(
    libraryDependencies ++=
      "io.taig" %% "data-core" % Version.Data ::
        "io.taig" %% "enumeration-ext-core" % Version.EnumerationExt ::
        "io.taig" %% "validation-core" % Version.Validation ::
        "org.typelevel" %% "cats-core" % Version.Cats ::
        "org.typelevel" %% "cats-parse" % Version.CatsParse ::
        "dev.zio" %% "zio-test" % Version.Zio % Test ::
        "dev.zio" %% "zio-test-sbt" % Version.Zio % Test ::
        Nil
  )

/** JSON schema definitions */
lazy val coreJson = module(identifier = Some("core-json"))
  .dependsOn(core % "compile->compile;test->test")

/** JSON codecs for io.circe */
lazy val coreJsonCirce = module(identifier = Some("core-json-circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %% "circe-core" % Version.Circe ::
        "io.taig" %% "data-circe" % Version.Data ::
        Nil
  )
  .dependsOn(coreJson % "compile->compile;test->test")

/** JSON Schema documents rendered from a JSON schema
  *
  * A renderer, not an alphabet, so it needs no second module to interpret into: a JSON Schema is a JSON document, and
  * core-json-circe already says what one of those is. What varies is not the target library but the consumer -- draft
  * 2020-12, a strict structured output profile -- and that is a JsonSchemaProfile value rather than a module.
  */
lazy val coreJsonSchema = module(identifier = Some("core-json-schema"))
  .dependsOn(coreJsonCirce % "compile->compile;test->test")

/** Component extensions for org.typelevel / case-insensitive */
lazy val coreCaseInsensitive = module(identifier = Some("core-case-insensitive"))
  .settings(libraryDependencies += "io.taig" %% "validation-cistring" % Version.Validation)
  .dependsOn(core % "compile->compile;test->test", coreIron % "test->compile")

/** CSV schema definitions */
lazy val coreCsv = module(identifier = Some("core-csv"))
  .dependsOn(core % "compile->compile;test->test")

/** CSV codecs for fs2-data */
lazy val coreCsvFs2Data = module(identifier = Some("core-csv-fs2-data"))
  .settings(libraryDependencies += "org.gnieh" %% "fs2-data-csv" % Version.Fs2Data)
  .dependsOn(coreCsv % "compile->compile;test->test")

/** Component extensions for io.github.iltotore / iron */
lazy val coreIron = module(identifier = Some("core-iron"))
  .settings(libraryDependencies += "io.taig" %% "validation-iron" % Version.Validation)
  .dependsOn(core % "compile->compile;test->test")

/** java.time primitives */
lazy val coreJavaTime = module(identifier = Some("core-java-time"))
  .jsSettings(
    libraryDependencies ++=
      "io.github.cquiroz" %% "scala-java-time" % Version.ScalaJavaTime % Test ::
        "io.github.cquiroz" %% "scala-java-time-tzdb" % Version.ScalaJavaTime % Test ::
        Nil
  )
  .dependsOn(core % "compile->compile;test->test")

/** TypeScript source definitions and printer */
lazy val coreTypescript = module(identifier = Some("core-typescript"))
  .dependsOn(core % "compile->compile;test->test")

/** TypeScript vocabulary of the effect Schema module */
lazy val coreTypescriptEffect = module(identifier = Some("core-typescript-effect"))
  .dependsOn(coreTypescript % "compile->compile;test->test")

/** TypeScript renderers for JSON, whatever the target library
  *
  * The test dependency on `core-json-circe` is for its fixtures, which are the schemas every JSON interpreter is
  * measured against, and for asserting that a rendered shape agrees with the document circe writes.
  */
lazy val coreJsonTypescript = module(identifier = Some("core-json-typescript"))
  .dependsOn(
    coreJson % "compile->compile;test->test",
    coreTypescript % "compile->compile;test->test",
    coreJsonCirce % "test->test"
  )

/** effect Schema code generation for JSON */
lazy val coreJsonTypescriptEffect = module(identifier = Some("core-json-typescript-effect"))
  .dependsOn(coreJsonTypescript % "compile->compile;test->test", coreTypescriptEffect)

/** HTTP endpoint definitions
  *
  * An alphabet, but not a paired one: an endpoint's envelope -- its path, its query string, its headers -- is text and
  * nothing more, so the codecs that read and write it live here and every backend adapts its own request type to the
  * `Chain`s they speak. A body is the one part that cannot be a pure value, because its bytes arrive over time, so this
  * module describes a body and stops. What a sequence of bytes is, is the interpreter's word.
  */
lazy val http = module(identifier = Some("http"))
  .settings(libraryDependencies += "org.scodec" %% "scodec-bits" % Version.ScodecBits)
  .dependsOn(core % "compile->compile;test->test")

// One CI job per platform. A runner has the memory to link one of them, not both, and the two halves have nothing to
// say to each other -- `testFull` because `test` in sbt 2 is testQuick and would report most of this as nothing to run.
addCommandAlias("testJVM", modules.map(_.jvm.id + "/testFull").mkString("; "))
addCommandAlias("testJS", modules.map(_.js.id + "/testFull").mkString("; "))
