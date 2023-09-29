package io.taig.otter.munit

import cats.effect.SyncIO
import io.taig.otter.http.Endpoint
import munit.{CatsEffectSuite, Location}

abstract class OtterSuite extends CatsEffectSuite:
  def test(endpoint: Endpoint[?, ?], description: String)(body: => Any)(implicit loc: Location): Unit =
    test(Formatters.testMessage(endpoint, description))(body)(loc)

  def test(endpoint: Endpoint[?, ?])(body: => Any)(implicit loc: Location): Unit =
    test(endpoint, description = "")(body)(loc)

  extension [T](self: SyncIO[FunFixture[T]])
    def test(endpoint: Endpoint[?, ?], description: String)(
        body: T => Any
    )(implicit loc: Location): Unit =
      new SyncIOFunFixtureOps(self).test(Formatters.testMessage(endpoint, description))(body)(loc)

    def test(endpoint: Endpoint[?, ?])(body: T => Any)(implicit loc: Location): Unit =
      test(endpoint, description = "")(body)(loc)
