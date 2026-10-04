package io.taig.otter.http

import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeCheckErrors
import scala.compiletime.testing.typeChecks

object Http4sRequirementsTest extends ZIOSpecDefault:
  private inline val ClientPrelude = """
    import cats.effect.IO
    import cats.syntax.all.*
    import io.taig.otter.Json
    import io.taig.otter.http.*
    import io.taig.otter.http.codec.*
    import io.taig.otter.http.fixture.dsl.*
    import io.taig.otter.http.fixture.payload
    import org.http4s.implicits.*
    val transport = org.http4s.client.Client.fromHttpApp(org.http4s.HttpApp[IO](_ => IO.pure(org.http4s.Response[IO]())))
    val plain = endpoint(request(method.get, __), response(status.noContent))
    val jsonRequest = endpoint(request(method.post, __)(body.json(payload.string)), response(status.noContent))
    val jsonResponse = endpoint(request(method.get, __), response(status.ok)(body.json(payload.string)))
    val error = response(Status(503))(body.json(payload.string)).dimap[Failure, String](_ => "failed")(identity)
    val overridden = plain.withErrors(ErrorOverrides(unexpected = Some(error)))
    val api = Api(ErrorPolicy.default, UnroutedPolicy.default)
    val errors = Api(ErrorPolicy.default.copy(unexpected = error), UnroutedPolicy.default)
    val answer = response(Status(404))(body.json(payload.string)).dimap[Unrouted, String](_ => "unrouted")(identity)
    val unrouted = Api(ErrorPolicy.default, UnroutedPolicy(answer, answer))
    val client = Http4s.client(Http4sPayload.Empty, uri"http://test", transport)
    val jsonClient = Http4s.client(Http4sCirce.Payload, uri"http://test", transport)
  """

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sRequirementsTest")(
    test("configured clients infer each function before an expected type is supplied") {
      val errors = typeCheckErrors(Http4sRequirementsTest.ClientPrelude + """
        final class TypeOf[A](value: A):
          def is[B](using A =:= B): Boolean = true
        def inferred[A](value: A): TypeOf[A] = new TypeOf(value)
        val fetch = jsonClient(jsonResponse)
        val write = jsonClient(jsonRequest)
        val asymmetric: Endpoint.Schema[Body.Whole[Json.Node], String, Int, Boolean, Long] =
          endpoint(
            request(method.post, __)(body.json(payload.string.dimap[String, Int](identity)(_.length))),
            response(status.ok)(body.json(payload.long.dimap[Boolean, Long](if _ then 1L else 0L)(identity)))
          )
        val differentSides = jsonClient(asymmetric)
        val standalone = jsonClient(overridden)
        val configured = jsonClient.withApi(api)
        val inherited = configured(jsonResponse)
        val local = configured(overridden)
        val explicit = jsonClient(overridden.compose(ErrorPolicy.default).client)
        inferred(fetch).is[Unit => IO[String]]
        inferred(write).is[String => IO[Unit]]
        inferred(differentSides).is[String => IO[Long]]
        inferred(standalone).is[Unit => IO[Either[Status | String, Unit]]]
        inferred(inherited).is[Unit => IO[Either[Status, String]]]
        inferred(local).is[Unit => IO[Either[Status | String, Unit]]]
        inferred(explicit).is[Unit => IO[Either[Status | String, Unit]]]
      """)
      assertTrue(errors.isEmpty)
    },
    test("a configured interpreter cannot widen to cover request, response, or override requirements") {
      val prelude = Http4sRequirementsTest.ClientPrelude
      assertTrue(
        typeChecks(prelude + "client(plain)"),
        typeChecks(prelude + "client.withApi(api)(plain)"),
        typeChecks(prelude + "jsonClient(jsonRequest); jsonClient(jsonResponse); jsonClient(overridden)"),
        typeChecks(prelude + "jsonClient.withApi(api)(overridden)"),
        !typeChecks(prelude + "client(jsonRequest)"),
        !typeChecks(prelude + "client(jsonResponse)"),
        !typeChecks(prelude + "client(overridden)"),
        !typeChecks(prelude + "client.withApi(api)(jsonRequest)"),
        !typeChecks(prelude + "client.withApi(api)(jsonResponse)"),
        !typeChecks(prelude + "client.withApi(api)(overridden)")
      )
    },
    test("binding an API checks both its error and unrouted requirements") {
      val prelude = Http4sRequirementsTest.ClientPrelude
      assertTrue(
        !typeChecks(prelude + "client.withApi(errors)"),
        !typeChecks(prelude + "client.withApi(unrouted)"),
        typeChecks(prelude + "jsonClient.withApi(errors)(plain)"),
        typeChecks(prelude + "jsonClient.withApi(unrouted)(plain)")
      )
    },
    test("configured clients accept binary bodies but reject multipart and streamed endpoints") {
      val prelude = Http4sRequirementsTest.ClientPrelude
      assertTrue(
        typeChecks(
          prelude + "client(endpoint(request(method.post, __)(body.binary), response(status.ok)(body.binary)))"
        ),
        !typeChecks(prelude + "jsonClient(Http4sRoundTripTest.multipart)"),
        !typeChecks(prelude + "jsonClient(Http4sRoundTripTest.streaming)"),
        !typeChecks(prelude + "jsonClient.withApi(api)(Http4sRoundTripTest.multipart)"),
        !typeChecks(prelude + "jsonClient.withApi(api)(Http4sRoundTripTest.streaming)")
      )
    },
    test("error payload requirements are retained when composing a body-free endpoint") {
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import cats.syntax.all.*
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.http.fixture.payload
        val error = response(Status(503))(body.json(payload.string)).dimap[Failure, String](_ => "failed")(identity)
        val e = ErrorPolicy.default.copy(unexpected = error)(endpoint(request(method.get, __), response(status.noContent)))
        Http4s.routes[IO](Route.composed(e, (_: Unit) => IO.unit))(Http4sPayload.Empty)
      """))
    },
    test("composed routes keep domain handler signatures and accept error interpreters") {
      val errors = typeCheckErrors("""
        import cats.effect.IO
        import cats.syntax.all.*
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.http.fixture.payload
        val error = response(Status(503))(body.json(payload.string)).dimap[Failure, String](_ => "failed")(identity)
        val e = ErrorPolicy.default.copy(unexpected = error)(endpoint(request(method.get, __), response(status.noContent)))
        Http4s.routes[IO](Route.composed(e, (_: Unit) => IO.unit))(Http4sCirce.Payload)
      """)
      assertTrue(errors.isEmpty)
    },
    test("endpoint overrides retain payload requirements") {
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import cats.syntax.all.*
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.http.fixture.payload
        val error = response(Status(503))(body.json(payload.string)).dimap[Failure, String](_ => "failed")(identity)
        val e = endpoint(request(method.get, __), response(status.noContent)).withErrors(ErrorOverrides(unexpected = Some(error)))
        Http4s.routes[IO](Api(ErrorPolicy.default, UnroutedPolicy.default), Route(e, (_: Unit) => IO.unit))(Http4sPayload.Empty)
      """))
    },
    test("endpoint overrides are accepted by the interpreter that covers them") {
      val errors = typeCheckErrors("""
        import cats.effect.IO
        import cats.syntax.all.*
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.http.fixture.payload
        val error = response(Status(503))(body.json(payload.string)).dimap[Failure, String](_ => "failed")(identity)
        val e = endpoint(request(method.get, __), response(status.noContent)).withErrors(ErrorOverrides(unexpected = Some(error)))
        Http4s.routes[IO](Api(ErrorPolicy.default, UnroutedPolicy.default), Route(e, (_: Unit) => IO.unit))(Http4sCirce.Payload)
      """)
      assertTrue(errors.isEmpty)
    },
    test("a handler untuples what a composed request holds, however its route is built") {
      val prelude = """
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        val e = endpoint(request(method.get, __ / segment("a", int) / segment("b", string)), response(status.noContent))
        def handle(a: Int, b: String): IO[Unit] = IO.unit
      """
      assertTrue(
        typeCheckErrors(prelude + "Route(e, (a, b) => handle(a, b))").isEmpty,
        typeCheckErrors(prelude + "Route(e.withErrors(ErrorOverrides()), (a, b) => handle(a, b))").isEmpty,
        typeCheckErrors(
          prelude + "Route(Api(ErrorPolicy.default, UnroutedPolicy.default), e, (a, b) => handle(a, b))"
        ).isEmpty,
        typeCheckErrors(prelude + "Route.composed(ErrorPolicy.default(e), (a, b) => handle(a, b))").isEmpty
      )
    },
    test("unrouted payload requirements are checked wherever the API is interpreted") {
      val prelude = """
        import cats.effect.IO
        import cats.syntax.all.*
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.http.fixture.payload
        val answer = response(Status(404))(body.json(payload.string)).dimap[Unrouted, String](_ => "unrouted")(identity)
        val api = Api(ErrorPolicy.default, UnroutedPolicy(answer, answer))
        val e = endpoint(request(method.get, __), response(status.noContent))
      """
      assertTrue(
        !typeChecks(prelude + "Http4s.app[IO](api, Route(e, (_: Unit) => IO.unit))(Http4sPayload.Empty)"),
        !typeChecks(prelude + "Http4s.fallback[IO](api, Route(e, (_: Unit) => IO.unit))(Http4sPayload.Empty)"),
        !typeChecks(prelude + "Http4s.routes[IO](api, Route(e, (_: Unit) => IO.unit))(Http4sPayload.Empty)"),
        typeChecks(prelude + "Http4s.app[IO](api, Route(e, (_: Unit) => IO.unit))(Http4sCirce.Payload)"),
        typeChecks(prelude + "Http4s.fallback[IO](api, Route(e, (_: Unit) => IO.unit))(Http4sCirce.Payload)"),
        typeChecks(prelude + "Http4s.routes[IO](api, Route(e, (_: Unit) => IO.unit))(Http4sCirce.Payload)")
      )
    },
    test("one unrouted writer fills both answers, and leaves the API's error type alone") {
      val errors = typeCheckErrors("""
        import cats.syntax.all.*
        import io.taig.otter.Json
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.http.fixture.payload
        val answer = response(Status(404))(body.json(payload.string)).dimap[Unrouted, String](_ => "unrouted")(identity)
        val error = response(Status(500))(body.json(payload.string)).dimap[Failure, String](_ => "failed")(identity)
        val policy: ErrorPolicy[Body.Whole[Json.Node], String] = ErrorPolicy(error, error, error, error, error, error, error, error)
        val api: Api[Body.Whole[Json.Node], String] = Api(policy, UnroutedPolicy(answer, answer))
      """)
      assertTrue(errors.isEmpty)
    },
    test("endpoint overrides preserve domain handler types") {
      val prelude = """
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.http.fixture.payload
        val e = endpoint(request(method.get, __), response(status.ok)(body.json(payload.string)))
          .withErrors(ErrorOverrides())
      """
      assertTrue(
        typeChecks(prelude + "Route(e, (_: Unit) => IO.pure(\"domain response\"))"),
        !typeChecks(prelude + "Route(e, (_: Unit) => IO.pure(1))"),
        !typeChecks(prelude + "Route(e, (_: Unit) => IO.pure(Right(\"domain response\")))")
      )
    },
    test("API clients retain global and endpoint-local error types") {
      val errors = typeCheckErrors("""
        import cats.effect.IO
        import cats.syntax.all.*
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.http.fixture.payload
        import org.http4s.implicits.*
        val error = response(Status(503))(body.json(payload.string)).dimap[Failure, String](_ => "failed")(identity)
        val e = endpoint(request(method.get, __), response(status.noContent)).withErrors(ErrorOverrides(unexpected = Some(error)))
        val client = org.http4s.client.Client.fromHttpApp(org.http4s.HttpApp[IO](_ => IO.pure(org.http4s.Response[IO]())))
        val call: Unit => IO[Either[Status | String, Unit]] =
          Http4s.client(Http4sCirce.Payload, uri"http://test", client).withApi(Api(ErrorPolicy.default, UnroutedPolicy.default))(e)
      """)
      assertTrue(errors.isEmpty)
    },
    test("client accepts its JSON interpreter") {
      assertTrue(typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.api
        import io.taig.otter.http.fixture.dsl.*
        import org.http4s.implicits.*
        val e = endpoint(request(method.post, __)(api.reported), response(status.noContent))
        val client = org.http4s.client.Client.fromHttpApp(org.http4s.HttpApp[IO](_ => IO.pure(org.http4s.Response[IO]())))
        Http4s.client(Http4sCirce.Payload, uri"http://test", client)(e)
      """))
    },
    test("optional request accepts its JSON interpreter") {
      assertTrue(typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.api
        import io.taig.otter.http.fixture.dsl.*
        val e = endpoint(request(method.post, __)(body.optional(api.reported)), response(status.noContent))
        Http4s.routes[IO](Route(e, (_: Option[io.taig.otter.http.fixture.Report]) => IO.unit))(Http4sCirce.Payload)
      """))
    },
    test("direct whole constructor accepts its JSON interpreter") {
      assertTrue(typeChecks("""
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.api
        import io.taig.otter.Reference
        val b = Body.Schema(Body.Value.Whole(MediaType("application", "json"), Reference.now(api.report)))
        Http4sBodyDecoder(Http4sCirce.Payload).decode(b, (None, scodec.bits.ByteVector.empty))
      """))
    },
    test("client requires its JSON interpreter") {
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.api
        import io.taig.otter.http.fixture.dsl.*
        import org.http4s.implicits.*
        val e = endpoint(request(method.post, __)(api.reported), response(status.noContent))
        val client = org.http4s.client.Client.fromHttpApp(org.http4s.HttpApp[IO](_ => IO.pure(org.http4s.Response[IO]())))
        Http4s.client(Http4sPayload.Empty, uri"http://test", client)(e)
      """))
    },
    test("optional request retains JSON requirements") {
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.api
        import io.taig.otter.http.fixture.dsl.*
        val e = endpoint(request(method.post, __)(body.optional(api.reported)), response(status.noContent))
        Http4s.routes[IO](Route(e, (_: Option[io.taig.otter.http.fixture.Report]) => IO.unit))(Http4sPayload.Empty)
      """))
    },
    test("direct whole constructor retains JSON requirement") {
      assertTrue(!typeChecks("""
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.api
        import io.taig.otter.Reference
        val b = Body.Schema(Body.Value.Whole(MediaType("application", "json"), Reference.now(api.report)))
        Http4sBodyDecoder(Http4sPayload.Empty).decode(b, (None, scodec.bits.ByteVector.empty))
      """))
    },
    test("direct streamed constructors cannot be decoded") {
      assertTrue(!typeChecks("""
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.api
        import io.taig.otter.Reference
        val body = Body.Schema(Body.Value.Streamed(MediaType("application", "json"), Frame.Lines, Reference.now(api.report)))
        Http4sBodyDecoder(Http4sCirce.Payload).decode(body, (None, scodec.bits.ByteVector.empty))
      """))
    },
    test("widened bodies cannot be decoded") {
      assertTrue(!typeChecks("""
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.*
        import io.taig.otter.http.fixture.api
        val body: Body.Node[Nothing, io.taig.otter.http.fixture.Report] = api.reported
        Http4sBodyDecoder(Http4sCirce.Payload).decode(body, (None, scodec.bits.ByteVector.empty))
      """))
    }
  )
