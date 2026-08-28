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
      name := "otter" + identifier.fold("")("-" + _),
      testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
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

noPublishSettings

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
  .aggregate(core, coreJson, coreJsonCirce)

/** Format agnostic schema definitions and interpreters */
lazy val core = module(identifier = Some("core"))
  .settings(
    Compile / sourceGenerators += Def.task {
      val file = (Compile / sourceManaged).value / "ConvertInstances.scala"
      IO.write(file, ConvertSourceGenerators.sumInstances(organization.value + ".otter"))
      Seq(file)
    }.taskValue,
    libraryDependencies ++=
      "io.taig" %% "data-core" % Version.Data ::
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
