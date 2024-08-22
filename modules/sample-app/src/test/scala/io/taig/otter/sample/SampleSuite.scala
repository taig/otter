package io.taig.otter.sample

import cats.effect.{IO, Resource, SyncIO}
import munit.{Location, TestOptions}
import io.taig.otter.http.Client
import io.taig.otter.munit.Suite
import io.taig.otter.sample.api.AuthenticatedEndpoint
import io.taig.otter.sample.app.SampleApp

abstract class SampleSuite extends Suite with SampleAssertions:
  def test(endpoint: AuthenticatedEndpoint[?, ?, ?], description: String)(body: => Any)(implicit loc: Location): Unit =
    test(endpoint.toAuthenticatedEndpoint, description)(body)(loc)

  def test(endpoint: AuthenticatedEndpoint[?, ?, ?])(body: => Any)(implicit loc: Location): Unit =
    test(endpoint, description = "")(body)(loc)

  implicit open class SyncIOFunFixtureSampleOps[T](private val self: SyncIO[FunFixture[T]])
      extends SyncIOFunFixtureOtterOps(self):
    def test(endpoint: AuthenticatedEndpoint[?, ?, ?], options: TestOptions)(
        body: T => Any
    )(implicit loc: Location): Unit = test(endpoint.toAuthenticatedEndpoint, options)(body)(loc)

    def test(endpoint: AuthenticatedEndpoint[?, ?, ?], description: String)(body: T => Any)(implicit
        loc: Location
    ): Unit = test(endpoint.toAuthenticatedEndpoint, description)(body)(loc)

    def test(endpoint: AuthenticatedEndpoint[?, ?, ?])(body: T => Any)(implicit loc: Location): Unit =
      test(endpoint, description = "")(body)(loc)

  val app: SyncIO[FunFixture[SampleContext]] =
    val context = SampleApp()
      .map(app => new SampleClient(Client(app)))
      .map(client => SampleContext(client, SampleApi(client)))

    ResourceFixture(Resource.eval(context))
