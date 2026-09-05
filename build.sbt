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
  .aggregate(modules.appended(benchmark) *)

/** Every module that cross builds, which is every module but the root: what there is to test, twice over. */
lazy val modules: List[CrossProject] = List(
  core,
  coreCaseInsensitive,
  coreCsv,
  coreCsvFs2Data,
  coreIron,
  coreJavaTime,
  coreJson,
  coreJsonBorer,
  coreJsonCirce,
  coreJsonSchema,
  coreJsonTypescript,
  coreJsonTypescriptEffect,
  coreTypescript,
  coreTypescriptEffect,
  http,
  httpJson,
  httpHttp4s,
  httpHttp4sCirce,
  httpOpenapi
)

/** JMH benchmarks
  *
  * Not one of the [[modules]], which are what cross builds and what `testJVM` and `testJS` run: this publishes nothing,
  * links nothing and is not a test. Aggregated all the same, so that compiling, formatting and linting the build still
  * reach it. It measures the JSON fixtures, which live in `core-json`'s test sources, and parses text, which
  * `circe-parser` is the only dependency here for.
  */
lazy val benchmark = module(identifier = Some("benchmark"), jvmOnly = true)
  .enablePlugins(JmhPlugin)
  .settings(noPublishSettings)
  .settings(libraryDependencies += "io.circe" %% "circe-parser" % Version.Circe)
  .dependsOn(coreJson % "compile->test", coreJsonBorer, coreJsonCirce)

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

/** JSON schema definitions, and the conformance suite every interpreter of them is held to
  *
  * The test sources here are a contract rather than a suite of their own. `JsonInterpreter` names the three points an
  * interpreter is measured at, `JsonDecoderSuite`, `JsonEncoderSuite` and `JsonRoundTripSuite` are what every one of
  * them must pass, and the fixtures are the schemas -- not one library's reading of them, which is why they live here
  * and not beside an interpreter.
  */
lazy val coreJson = module(identifier = Some("core-json"))
  .dependsOn(core % "compile->compile;test->test")

/** JSON codecs for io.bullet borer
  *
  * The core-json/core-json-circe pairing again, against a library of a different shape, and the shape is the whole
  * interest of it. borer has a document model, which is what makes an interpreter possible at all -- a schema driven
  * read needs random access, because a record reads its members by name and a union retries its branches, and neither
  * borer's reader nor any other streaming reader can be asked to go back. But borer has no `Either` returning decoder
  * typeclass and no cursor history, so the bridge object is where the two modules genuinely differ rather than mirror.
  * And it writes into a `Writer` rather than building a value, which is why this module's encoder carries a deferred
  * write and no document is built on the way out.
  *
  * The test dependency on core-json-circe is for its fixtures and for the circe interpreter itself: the strongest claim
  * this module can make is that it reads a document exactly as circe reads it, violation tree and all.
  */
lazy val coreJsonBorer = module(identifier = Some("core-json-borer"))
  .settings(
    libraryDependencies ++=
      "io.bullet" %% "borer-core" % Version.Borer ::
        "io.circe" %% "circe-parser" % Version.Circe % Test ::
        Nil
  )
  .dependsOn(coreJson % "compile->compile;test->test", coreJsonCirce % "test->test")

/** JSON codecs for io.circe */
lazy val coreJsonCirce = module(identifier = Some("core-json-circe"))
  .settings(
    libraryDependencies ++=
      "io.circe" %% "circe-core" % Version.Circe ::
        "io.taig" %% "data-circe" % Version.Data ::
        "io.circe" %% "circe-parser" % Version.Circe % Test ::
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
  .dependsOn(coreJson % "test->test", coreJsonCirce)

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
  * The test dependency on `core-json` is for its fixtures, which are the schemas every JSON interpreter and every
  * renderer of one is measured against. No interpreter is named here, because a rendered shape is a claim about the
  * schema rather than about any library's reading of it.
  */
lazy val coreJsonTypescript = module(identifier = Some("core-json-typescript"))
  .dependsOn(coreJson % "compile->compile;test->test", coreTypescript % "compile->compile;test->test")

/** effect Schema code generation for JSON
  *
  * The test dependency on `core-json-circe` is where an interpreter *is* named: this is the module that asserts a
  * generated type accepts exactly the documents circe writes, so it is the one that says so in its build.
  */
lazy val coreJsonTypescriptEffect = module(identifier = Some("core-json-typescript-effect"))
  .dependsOn(
    coreJsonTypescript % "compile->compile;test->test",
    coreTypescriptEffect,
    coreJsonCirce % "test->test"
  )

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

/** JSON payloads for HTTP bodies
  *
  * A module rather than a few lines in `http`, so that describing an endpoint does not drag in a JSON alphabet. A
  * payload is any schema at all, and this is what says one of them may be a JSON one.
  */
lazy val httpJson = module(identifier = Some("http-json"))
  .dependsOn(http % "compile->compile;test->test", coreJson % "compile->compile;test->test")

/** Endpoints served and called through org.http4s
  *
  * The interpreter the pairing said could not exist, and the reason it can is that http4s draws the distinction this
  * module needs: an `Entity` is `Strict` over a `ByteVector`, `Streamed` over an fs2 stream, or `Empty`, and the first
  * and last of those are `Entity[Pure]`. So a whole body is a pure value after all -- not a shared document model, but
  * enough of one that reading and writing it needs no effect type, and the codecs here are the same `Encoder` and
  * `Decoder` every other alphabet is interpreted by. Only a streamed body has to mention `F`.
  *
  * The envelope needs no adapting worth the name: `Query` is `Vector[(String, Option[String])]` in both directions and
  * `Headers` is a list of raw name and value pairs, which is what `http`'s codecs already speak. That they line up this
  * exactly is the evidence the wire slices were drawn in the right place.
  *
  * `http4s-server` is deliberately absent. `HttpRoutes` lives in `http4s-core`, and nothing here builds a server or
  * routes between several -- what to listen on is the caller's, and one fewer dependency is one fewer thing pinned to a
  * milestone.
  */
lazy val httpHttp4s = module(identifier = Some("http-http4s"))
  .settings(
    libraryDependencies ++=
      "org.http4s" %% "http4s-core" % Version.Http4s ::
        "org.http4s" %% "http4s-client" % Version.Http4s ::
        Nil
  )
  .dependsOn(http % "compile->compile;test->test")

/** JSON payloads read and written by io.circe, for the http4s interpreter
  *
  * A module of its own where `http-openapi` needed none, and the difference is what circe is being asked for. There,
  * circe is the document model a JSON Schema *is*, so naming it settles nothing a caller would want to choose. Here it
  * is one of two interpreters of the same alphabet, and `core-json-borer` measurably writes faster while `circe` reads
  * faster -- a trade an HTTP server is entitled to make for itself. A `http-http4s-borer` sits beside this one.
  */
lazy val httpHttp4sCirce = module(identifier = Some("http-http4s-circe"))
  .settings(libraryDependencies += "io.circe" %% "circe-parser" % Version.Circe)
  .dependsOn(
    httpHttp4s % "compile->compile;test->test",
    coreJsonCirce % "compile->compile;test->test",
    httpJson % "compile->compile;test->test"
  )

/** OpenAPI documents rendered from an endpoint
  *
  * A renderer, on the same reasoning `core-json-schema` is one: an OpenAPI document is a JSON document, and
  * `core-json-circe` already says what one of those is. The payload schemas inside it are rendered by a parameter, so a
  * payload alphabet other than JSON contributes its own renderer rather than being named here.
  */
lazy val httpOpenapi = module(identifier = Some("http-openapi"))
  .dependsOn(httpJson % "compile->compile;test->test", coreJsonSchema % "compile->compile;test->test")

// One CI job per platform. A runner has the memory to link one of them, not both, and the two halves have nothing to
// say to each other -- `testFull` because `test` in sbt 2 is testQuick and would report most of this as nothing to run.
addCommandAlias("testJVM", modules.map(_.jvm.id + "/testFull").mkString("; "))
addCommandAlias("testJS", modules.map(_.js.id + "/testFull").mkString("; "))
