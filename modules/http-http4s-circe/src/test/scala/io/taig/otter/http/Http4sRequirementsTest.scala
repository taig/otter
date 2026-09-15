package io.taig.otter.http

import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeCheckErrors
import scala.compiletime.testing.typeChecks

object Http4sRequirementsTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sRequirementsTest")(
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
        Http4s.routes[IO](Route(e, (_: Unit) => IO.unit))(Http4sPayload.Empty)
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
        Http4s.routes[IO](Route(e, (_: Unit) => IO.unit))(Http4sCirce.Payload)
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
        Http4s.routes[IO](Api(ErrorPolicy.default), Route(e, (_: Unit) => IO.unit))(Http4sPayload.Empty)
      """))
    },
    test("endpoint overrides preserve domain handler types") {
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        val e = endpoint(request(method.get, __), response(status.noContent)).withErrors(ErrorOverrides())
        Route(e, (_: Unit) => IO.pure("wrong domain response"))
      """))
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
          Http4s.client[IO, Unit, Unit](Api(ErrorPolicy.default), e)(Http4sCirce.Payload, uri"http://test", client)
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
        Http4s.client[IO, io.taig.otter.http.fixture.Report, Unit](e)(Http4sCirce.Payload, uri"http://test", client)
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
        Http4s.client[IO, io.taig.otter.http.fixture.Report, Unit](e)(Http4sPayload.Empty, uri"http://test", client)
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
