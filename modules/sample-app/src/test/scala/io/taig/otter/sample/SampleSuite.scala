// package io.taig.otter.sample

// import cats.effect.{IO, Resource, SyncIO}
// import io.taig.otter.http.AppClient
// import io.taig.otter.munit.OtterSuite
// import io.taig.otter.sample.api.endpoints.AuthenticatedEndpoint
// import munit.{Location, TestOptions}

// abstract class SampleSuite extends OtterSuite with SampleExtensions with SampleAssertions:
//   def test(endpoint: AuthenticatedEndpoint[?, ?, ?], description: String)(body: => Any)(implicit loc: Location): Unit =
//     test(endpoint.toAuthenticatedEndpoint, description)(body)(loc)

//   def test(endpoint: AuthenticatedEndpoint[?, ?, ?])(body: => Any)(implicit loc: Location): Unit =
//     test(endpoint, description = "")(body)(loc)

//   implicit open class SyncIOFunFixtureSampleOps[T](private val self: SyncIO[FunFixture[T]])
//       extends SyncIOFunFixtureOtterOps(self):
//     def test(endpoint: AuthenticatedEndpoint[?, ?, ?], options: TestOptions)(
//         body: T => Any
//     )(implicit loc: Location): Unit = test(endpoint.toAuthenticatedEndpoint, options)(body)(loc)

//     def test(endpoint: AuthenticatedEndpoint[?, ?, ?], description: String)(body: T => Any)(implicit
//         loc: Location
//     ): Unit = test(endpoint.toAuthenticatedEndpoint, description)(body)(loc)

//     def test(endpoint: AuthenticatedEndpoint[?, ?, ?])(body: T => Any)(implicit loc: Location): Unit =
//       test(endpoint, description = "")(body)(loc)

//   val app: SyncIO[FunFixture[SampleContext]] =
//     val context = SampleApp
//       .create(logger = _ => IO.unit)
//       .map(app => new SampleClient(AppClient(app)))
//       .map(client => SampleContext(client, SampleApi(client)))

//     ResourceFixture(Resource.eval(context))
