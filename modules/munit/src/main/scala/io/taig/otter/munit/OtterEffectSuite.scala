package io.taig.otter.munit

import cats.effect.SyncIO
import cats.syntax.all.*
import io.taig.otter.http.Endpoint
import munit.CatsEffectSuite
import munit.Location
import munit.TestOptions

import scala.language.adhocExtensions

abstract class OtterEffectSuite extends CatsEffectSuite:
  def test(endpoint: Endpoint[?, ?, ?, ?, ?], description: String)(body: => Any)(implicit loc: Location): Unit =
    test(toMessage(endpoint, description))(body)(loc)

  def test(endpoint: Endpoint[?, ?, ?, ?, ?])(body: => Any)(implicit loc: Location): Unit =
    test(endpoint, description = "")(body)(loc)

  implicit open class SyncIOFunFixtureOtterOps[T](private val self: SyncIO[FunFixture[T]])
      extends SyncIOFunFixtureOps(self):
    def test(endpoint: Endpoint[?, ?, ?, ?, ?], options: TestOptions)(
        body: T => Any
    )(implicit loc: Location): Unit = test(options.withName(toMessage(endpoint, options.name)))(body)(loc)

    def test(endpoint: Endpoint[?, ?, ?, ?, ?], description: String)(
        body: T => Any
    )(implicit loc: Location): Unit = test(toMessage(endpoint, description))(body)(loc)

    def test(endpoint: Endpoint[?, ?, ?, ?, ?])(body: T => Any)(implicit loc: Location): Unit =
      test(endpoint, description = "")(body)(loc)

private def toMessage(endpoint: Endpoint[?, ?, ?, ?, ?], description: String): String =
  val request = endpoint.request
  val coordinates = show"${request.method} ${request.url.path}"
  if description.isEmpty then coordinates else s"$coordinates: $description"
