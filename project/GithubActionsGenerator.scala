import io.circe.Json
import io.circe.syntax._

object GithubActionsGenerator {
  object Step {
    def setupJava(version: String): Json = Json.obj(
      "name" := "Setup Java",
      "uses" := "actions/setup-java@v4",
      "with" := Json.obj(
        "distribution" := "temurin",
        "java-version" := version,
        "cache" := "sbt"
      )
    )

    val Checkout: Json = Json.obj(
      "name" := "Checkout",
      "uses" := "actions/checkout@v4",
      "with" := Json.obj(
        "fetch-depth" := 0
      )
    )

    val SetupSbt: Json = Json.obj(
      "name" := "Setup sbt",
      "uses" := "actions/setup-sbt@v1"
    )
  }

  object Job {
    def apply(name: String, javaVersion: String, mode: String = "DEV", needs: List[String] = Nil)(steps: Json*): Json =
      Json.obj(
        "name" := name,
        "runs-on" := "ubuntu-latest",
        "env" := Json.obj(
          s"SBT_TPOLECAT_$mode" := "true"
        ),
        "steps" := steps
      )

    def blowout(javaVersion: String): Json = Job(name = "Blowout", javaVersion)(
      Step.Checkout,
      Step.setupJava(javaVersion),
      Step.SetupSbt,
      Json.obj("run" := "sbt blowoutCheck")
    )

    def scalafmt(javaVersion: String): Json = Job(name = "Scalafmt", javaVersion)(
      Step.Checkout,
      Step.setupJava(javaVersion),
      Step.SetupSbt,
      Json.obj("run" := "sbt scalafmtCheckAll")
    )

    def scalafix(javaVersion: String): Json = Job(name = "Scalafix", javaVersion, mode = "CI")(
      Step.Checkout,
      Step.setupJava(javaVersion),
      Step.SetupSbt,
      Json.obj("run" := "sbt scalafixCheckAll")
    )

    def tests(javaVersion: String): Json = Job(name = "Tests", javaVersion)(
      Step.Checkout,
      Step.setupJava(javaVersion),
      Step.SetupSbt,
      Json.obj("run" := "sbt test")
    )

    def deploy(javaVersion: String): Json = Job(
      name = "Deploy",
      javaVersion,
      mode = "RELEASE",
      needs = List("blowout", "scalafmt", "scalafix", "tests")
    )(
      Step.Checkout,
      Step.setupJava(javaVersion),
      Step.SetupSbt,
      Json.obj(
        "name" := "Release",
        "run" := "sbt ci-release",
        "env" := Json.obj(
          "PGP_PASSPHRASE" := "${{secrets.PGP_PASSPHRASE}}",
          "PGP_SECRET" := "${{secrets.PGP_SECRET}}",
          "SONATYPE_PASSWORD" := "${{secrets.SONATYPE_PASSWORD}}",
          "SONATYPE_USERNAME" := "${{secrets.SONATYPE_USERNAME}}"
        )
      )
    )
  }

  def main(javaVersion: String): Json = Json.obj(
    "name" := "CI",
    "on" := Json.obj(
      "push" := Json.obj("branches" := List("main"))
    ),
    "jobs" := Json.obj(
      "blowout" := Job.blowout(javaVersion),
      "scalafmt" := Job.scalafmt(javaVersion),
      "scalafix" := Job.scalafix(javaVersion),
      "tests" := Job.tests(javaVersion),
      "deploy" := Job.deploy(javaVersion)
    )
  )

  def tag(javaVersion: String): Json = Json.obj(
    "name" := "CD",
    "on" := Json.obj(
      "push" := Json.obj("tags" := List("*.*.*"))
    ),
    "jobs" := Json.obj(
      "blowout" := Job.blowout(javaVersion),
      "scalafmt" := Job.scalafmt(javaVersion),
      "scalafix" := Job.scalafix(javaVersion),
      "tests" := Job.tests(javaVersion),
      "deploy" := Job.deploy(javaVersion)
    )
  )

  def pullRequest(javaVersion: String): Json = Json.obj(
    "name" := "CI",
    "on" := Json.obj(
      "pull_request" := Json.obj(
        "branches" := List("main")
      )
    ),
    "jobs" := Json.obj(
      "blowout" := Job.blowout(javaVersion),
      "scalafmt" := Job.scalafmt(javaVersion),
      "scalafix" := Job.scalafix(javaVersion),
      "tests" := Job.tests(javaVersion)
    )
  )
}
