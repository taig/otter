package io.taig.otter.http

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import org.http4s.Request as Http4sRequest
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.implicits.*
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

object Http4sApiTest extends ZIOSpecDefault:
  private val base: Uri = uri"http://otter.test"
  private def answer(status: Int, message: String) =
    response(Status(status))(body.json(payload.string)).dimap[Failure, String](_ => message)(identity)

  private val badRequest = answer(400, "global request error")
  private val serverError = answer(502, "global server error")
  private val policy = ErrorPolicy(
    badRequest,
    badRequest,
    badRequest,
    badRequest,
    serverError,
    serverError,
    serverError,
    serverError
  )
  private val api = Api(policy)
  private val inherited = endpoint(request(method.get, __), response(status.noContent))
  private val overridden = endpoint(request(method.get, __ / segment("id", int)), response(status.noContent))
    .withErrors(ErrorOverrides(unexpected = Some(answer(503, "local server error"))))
  private val cause = new IllegalStateException("handler failed")

  private def run[A](value: IO[A]): Task[A] = ZIO.fromFuture(_ => value.unsafeToFuture())

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sApiTest")(
    test("API routes apply defaults and local overrides without documentation membership"):
      val routes = Http4s
        .routes[IO](
          api,
          Route(inherited, (_: Unit) => IO.raiseError[Unit](cause)),
          Route(overridden, (_: Int) => IO.raiseError[Unit](cause))
        )(Http4sCirce.Payload)
        .orNotFound
      val client = Client.fromHttpApp(routes)
      run:
        for
          global <- Http4s.client[IO, Unit, Unit](api, inherited)(Http4sCirce.Payload, base, client)(())
          local <- Http4s.client[IO, Int, Unit](api, overridden)(Http4sCirce.Payload, base, client)(1)
          globalStatus <- routes.run(Http4sRequest[IO](uri = base)).map(_.status.code)
          localStatus <- routes.run(Http4sRequest[IO](uri = base / "1")).map(_.status.code)
          malformed <- routes.run(Http4sRequest[IO](uri = base / "invalid"))
          malformedBody <- Http4sEnvelope.toBytes(malformed.entity)
        yield assertTrue(
          api.endpoints.isEmpty,
          global == Left("global server error"),
          local == Left("local server error"),
          globalStatus == 502,
          localStatus == 503,
          malformed.status.code == 400,
          malformedBody.decodeUtf8 == Right("\"global request error\"")
        )
    ,
    test("individual API routes use the same composition as the route collection"):
      val routes = Http4s
        .routes[IO](
          Route(api, overridden, (_: Int) => IO.raiseError[Unit](cause))
        )(Http4sCirce.Payload)
        .orNotFound
      val client = Client.fromHttpApp(routes)
      run(Http4s.client[IO, Int, Unit](api, overridden)(Http4sCirce.Payload, base, client)(1))
        .map(answer => assertTrue(answer == Left("local server error")))
    ,
    test("standalone overrides inherit bodyless defaults and clients retain both error types"):
      val routes = Http4s
        .routes[IO](
          Route(overridden, (_: Int) => IO.raiseError[Unit](cause))
        )(Http4sCirce.Payload)
        .orNotFound
      val client = Client.fromHttpApp(routes)
      val call: Int => IO[Either[String | Status, Unit]] =
        Http4s.client[IO, Int, Unit](overridden)(Http4sCirce.Payload, base, client)
      run:
        for
          local <- call(1)
          malformed <- routes.run(Http4sRequest[IO](uri = base / "invalid"))
          bytes <- Http4sEnvelope.toBytes(malformed.entity)
        yield assertTrue(local == Left("local server error"), malformed.status.code == 400, bytes.isEmpty)
  )
