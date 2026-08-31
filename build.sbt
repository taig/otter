import sbtcrossproject.CrossProject

def module(identifier: Option[String], jvmOnly: Boolean = false): CrossProject = {
  val platforms = List(JVMPlatform) ++ (if (jvmOnly) Nil else List(JSPlatform))
  CrossProject(identifier.getOrElse("root"), file(identifier.fold(".")("modules/" + _)))(platforms *)
    .crossType(CrossType.Pure)
    .withoutSuffixFor(JVMPlatform)
    .build()
    .settings(
      Compile / console / scalacOptions -= "-Wunused:all",
      Compile / scalacOptions ++= "-source:future" :: "-rewrite" :: "-new-syntax" :: "-Wunused:all" ::
        "-Xmax-inlines" :: "64" :: Nil,
      name := "otter-next" + identifier.fold("")("-" + _)
    )
}

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
  .aggregate(core, coreCsv, coreCsvFs2Data, coreJavaTime, coreJson, coreJsonCirce)

/** Format agnostic schema definitions and interpreters */
lazy val core = module(identifier = Some("core"))
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

/** CSV schema definitions */
lazy val coreCsv = module(identifier = Some("core-csv"))
  .dependsOn(core % "compile->compile;test->test")

/** CSV codecs for fs2-data */
lazy val coreCsvFs2Data = module(identifier = Some("core-csv-fs2-data"))
  .settings(libraryDependencies += "org.gnieh" %% "fs2-data-csv" % Version.Fs2Data)
  .dependsOn(coreCsv % "compile->compile;test->test")

/** java.time primitives */
lazy val coreJavaTime = module(identifier = Some("core-java-time"))
  .jsSettings(
    libraryDependencies ++=
      "io.github.cquiroz" %% "scala-java-time" % Version.ScalaJavaTime % Test ::
        "io.github.cquiroz" %% "scala-java-time-tzdb" % Version.ScalaJavaTime % Test ::
        Nil
  )
  .dependsOn(core % "compile->compile;test->test")
