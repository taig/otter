package io.taig.otter.sample.routes

import cats.effect.{IO, Resource, SyncIO}
import io.taig.otter.http.App
import io.taig.otter.munit.{Formatters, OtterSuite}
import io.taig.otter.sample.SampleApp
import io.taig.otter.sample.api.endpoints.Endpoint
import munit.{CatsEffectSuite, Location}

abstract class SampleSuite extends OtterSuite:
  def test(endpoint: Endpoint[?, ?, ?], description: String)(body: => Any)(implicit loc: Location): Unit =
    test(endpoint.toUnauthenticatedEndpoint, description)(body)(loc)

  def test(endpoint: Endpoint[?, ?, ?])(body: => Any)(implicit loc: Location): Unit =
    test(endpoint, description = "")(body)(loc)

  extension [T](self: SyncIO[FunFixture[T]])
    def test(endpoint: Endpoint[?, ?, ?], description: String)(body: T => Any)(implicit loc: Location): Unit =
      self.test(endpoint.toUnauthenticatedEndpoint, description)(body)(loc)

    def test(endpoint: Endpoint[?, ?, ?])(body: T => Any)(implicit loc: Location): Unit =
      test(endpoint, description = "")(body)(loc)

  val app: SyncIO[FunFixture[App[IO]]] = ResourceFixture(Resource.eval(SampleApp.create))
