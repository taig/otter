package io.taig.otter.sample

import cats.effect.IO
import cats.effect.Resource
import cats.effect.SyncIO
import io.taig.otter.http.Client
import io.taig.otter.munit.Assertions
import io.taig.otter.munit.Suite
import io.taig.otter.sample.api.RoleEndpoint
import io.taig.otter.sample.app.SampleApp
import munit.Location
import munit.TestOptions

abstract class SampleSuite extends Suite with Assertions:
  def test(endpoint: RoleEndpoint[?, ?, ?], description: String)(body: => Any)(implicit loc: Location): Unit =
    test(endpoint.toAuthenticatedEndpoint, description)(body)(loc)

  def test(endpoint: RoleEndpoint[?, ?, ?])(body: => Any)(implicit loc: Location): Unit =
    test(endpoint, description = "")(body)(loc)

  implicit open class SyncIOFunFixtureSampleOps[T](private val self: SyncIO[FunFixture[T]])
      extends SyncIOFunFixtureOtterOps(self):
    def test(endpoint: RoleEndpoint[?, ?, ?], options: TestOptions)(
        body: T => Any
    )(implicit loc: Location): Unit = test(endpoint.toAuthenticatedEndpoint, options)(body)(loc)

    def test(endpoint: RoleEndpoint[?, ?, ?], description: String)(body: T => Any)(implicit
        loc: Location
    ): Unit = test(endpoint.toAuthenticatedEndpoint, description)(body)(loc)

    def test(endpoint: RoleEndpoint[?, ?, ?])(body: T => Any)(implicit loc: Location): Unit =
      test(endpoint, description = "")(body)(loc)

  val app: SyncIO[FunFixture[SampleContext]] =
    val context = SampleApp(_ => IO.unit)
      .map(app => new SampleClient(Client(app)))
      .map(client => SampleContext(client, SampleApi(client)))

    ResourceFixture(Resource.eval(context))
