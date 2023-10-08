package io.taig.otter.munit

import cats.effect.SyncIO
import io.taig.otter.http.Endpoint
import munit.{CatsEffectSuite, Location}

abstract class OtterSuite extends CatsEffectSuite with OtterExtensions:
  def test(endpoint: Endpoint[?, ?], description: String)(body: => Any)(implicit loc: Location): Unit =
    test(toMessage(endpoint, description))(body)(loc)

  def test(endpoint: Endpoint[?, ?])(body: => Any)(implicit loc: Location): Unit =
    test(endpoint, description = "")(body)(loc)

  extension [T](self: SyncIO[FunFixture[T]])
    def test(endpoint: Endpoint[?, ?], description: String)(
        body: T => Any
    )(implicit loc: Location): Unit =
      new SyncIOFunFixtureOps(self).test(toMessage(endpoint, description))(body)(loc)

    def test(endpoint: Endpoint[?, ?])(body: T => Any)(implicit loc: Location): Unit =
      test(endpoint, description = "")(body)(loc)

private def toMessage(endpoint: Endpoint[?, ?], description: String): String =
  val request = endpoint.request
  val coordinates = s"${request.method} ${request.url.print}"
  if description.isEmpty then coordinates else s"$coordinates: $description"
