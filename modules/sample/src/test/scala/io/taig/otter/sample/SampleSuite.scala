package io.taig.otter.sample

import cats.effect.{IO, Resource, SyncIO}
import io.taig.otter.http.AppClient
import io.taig.otter.munit.OtterSuite
import io.taig.otter.sample.api.endpoints.Endpoint
import munit.Location

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

  val app: SyncIO[FunFixture[Context]] =
    val context = SampleApp
      .create(logger = _ => IO.unit)
      .map(app => new SampleClient(AppClient(app)))
      .map(Context.apply)

    ResourceFixture(Resource.eval(context))
