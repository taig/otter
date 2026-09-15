package io.taig.otter.http

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import io.taig.otter.http.fixture.*
import io.taig.otter.http.fixture.dsl.*
import org.http4s.Entity
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import org.http4s.Method as Http4sMethod
import org.http4s.Request as Http4sRequest
import org.http4s.Response as Http4sResponse
import org.http4s.Uri
import org.http4s.client.Client as Http4sClient
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

import scala.compiletime.testing.typeChecks

object Http4sEmptyBodyTest extends ZIOSpecDefault:
  private val Base: Uri = uri"http://otter.test"
  private val Png: MediaType = MediaType("image", "png")
  private val png: Body.Of[Body.Opaque, ByteVector] = body.binary(Png)
  private val text: Body.Of[Body.Opaque, ByteVector] = body.binary(dsl.mediaType.text)
  private val bodyFree: Endpoint.Of[Body.Or[Nothing, Nothing], Unit, Unit] = endpoint(
    request(method.get, __),
    response(status.noContent).toUnion
  )

  private val binaryOnly: Endpoint.Of[Body.Or[Body.Opaque, Nothing], ByteVector, Unit] = endpoint(
    request(method.post, __)(png),
    response(status.noContent).toUnion
  )

  private val EmptyEntities: List[(String, Entity[IO])] = List(
    "empty" -> Entity.empty,
    "strict" -> Entity.Strict(ByteVector.empty),
    "streamed" -> Entity.Streamed(Stream.empty, None)
  )

  private val upload: Endpoint.Of[dsl.Payload, Option[ByteVector], Unit] = endpoint(
    request(method.post, __)(body.optional(png)),
    response(status.noContent).toUnion
  )

  private val download: Endpoint.Of[dsl.Payload, Unit, ByteVector] = endpoint(
    request(method.get, __),
    response(status.ok)(png).toUnion
  )

  private val alternatives: Endpoint.Of[dsl.Payload, Unit, Either[ByteVector, ByteVector]] = endpoint(
    request(method.get, __),
    response(status.ok)(png) :+ response(status.ok)(text)
  )

  private def headers(contentType: Option[String]): Http4sHeaders =
    Http4sHeaders(contentType.toList.map(value => Http4sHeader.Raw(CIString("Content-Type"), value)))

  private def received[A](
      endpoint: Endpoint.Of[dsl.Payload, A, Unit],
      request: Http4sRequest[IO]
  ): Task[(Int, Option[A])] =
    ZIO.fromFuture: _ =>
      IO.ref(Option.empty[A])
        .flatMap: ref =>
          Http4s
            .routes[IO](Route(endpoint, (value: A) => ref.set(Some(value))))(Http4sCirce.Payload)
            .orNotFound
            .run(request)
            .flatMap(response => ref.get.map((response.status.code, _)))
        .unsafeToFuture()

  private def decoded[A](
      endpoint: Endpoint.Of[dsl.Payload, Unit, A],
      response: Http4sResponse[IO]
  ): Task[Either[Throwable, A]] =
    ZIO.fromFuture: _ =>
      val client = Http4sClient.fromHttpApp[IO](org.http4s.HttpApp[IO](_ => IO.pure(response)))

      Http4s.client[IO, Unit, A](endpoint)(Http4sCirce.Payload, Base, client)(()).attempt.unsafeToFuture()

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sEmptyBodyTest")(
    test("the empty interpreter supports body-free and binary-only routes"):
      assertTrue(typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.codec.Http4sPayload
        import scodec.bits.ByteVector
        Http4s.routes[IO](Route(Http4sEmptyBodyTest.bodyFree, (_: Unit) => IO.unit))(Http4sPayload.Empty)
        Http4s.routes[IO](Route(Http4sEmptyBodyTest.binaryOnly, (_: ByteVector) => IO.unit))(Http4sPayload.Empty)
      """))
    ,
    suite("raw empty messages")(
      EmptyEntities.map { (name, entity) =>
        suite(name)(
          test("a typed empty request is a present binary value"):
            val request = Http4sRequest[IO](
              method = Http4sMethod.POST,
              uri = Base,
              headers = headers(Some("IMAGE/PNG; version=1")),
              entity = entity
            )

            received(upload, request).map(value => assertTrue(value == (204, Some(Some(ByteVector.empty)))))
          ,
          test("an untyped empty request is absent, including with Content-Length: 0"):
            val request = Http4sRequest[IO](
              method = Http4sMethod.POST,
              uri = Base,
              headers = Http4sHeaders(Http4sHeader.Raw(CIString("Content-Length"), "0")),
              entity = entity
            )

            received(upload, request).map(value => assertTrue(value == (204, Some(None))))
          ,
          test("a typed empty request must match its declared media type"):
            val request = Http4sRequest[IO](
              method = Http4sMethod.POST,
              uri = Base,
              headers = headers(Some("text/plain")),
              entity = entity
            )

            received(upload, request).map((code, value) => assertTrue(code == 415, value.isEmpty))
          ,
          test("a typed empty response is accepted under the declared media type"):
            decoded(download, Http4sResponse[IO](headers = headers(Some("IMAGE/PNG; version=1")), entity = entity))
              .map(value => assertTrue(value == Right(ByteVector.empty)))
          ,
          test("a typed empty response must match its declared media type"):
            decoded(download, Http4sResponse[IO](headers = headers(Some("text/plain")), entity = entity))
              .map(value =>
                assertTrue(value.left.exists {
                  case Http4sFailure.Response(violations) => Http4s.report(violations).contains("text/plain")
                  case _                                  => false
                })
              )
          ,
          test("an empty response selects the alternative named by its media type"):
            decoded(alternatives, Http4sResponse[IO](headers = headers(Some("text/plain")), entity = entity))
              .map(value => assertTrue(value == Right(Right(ByteVector.empty))))
          ,
          test("an untyped empty response remains readable as empty binary content"):
            decoded(download, Http4sResponse[IO](entity = entity))
              .map(value => assertTrue(value == Right(ByteVector.empty)))
        )
      }*
    ),
    test("the client emits a content type for Some(empty) and none for None"):
      ZIO
        .foreach(List(Some(ByteVector.empty), None)): value =>
          ZIO.fromFuture: _ =>
            IO.ref(Option.empty[(Option[MediaType], ByteVector)])
              .flatMap: ref =>
                val client = Http4sClient.fromHttpApp[IO](org.http4s.HttpApp[IO]: request =>
                  Http4sEnvelope
                    .toBytes(request.entity)
                    .flatMap(bytes => ref.set(Some((Http4sEnvelope.toMediaType(request.headers), bytes))))
                    .as(Http4sResponse[IO](status = org.http4s.Status.NoContent)))

                Http4s.client[IO, Option[ByteVector], Unit](upload)(Http4sCirce.Payload, Base, client)(value) *> ref.get
              .map(seen => assertTrue(seen == Some((value.map(_ => Png), ByteVector.empty))))
              .unsafeToFuture()
        .map(results => results.reduce(_ && _))
    ,
    test("a nonempty untyped optional request is present"):
      val bytes = ByteVector(1, 2, 3)
      val request = Http4sRequest[IO](method = Http4sMethod.POST, uri = Base, entity = Entity.strict(bytes))

      received(upload, request).map(value => assertTrue(value == (204, Some(Some(bytes)))))
    ,
    test("an empty JSON response is decoded and rejected as invalid JSON"):
      val endpoint = io.taig.otter.http.fixture.dsl.endpoint(
        request(method.get, __),
        response(status.ok)(body.json(api.settings)).toUnion
      )

      decoded(endpoint, Http4sResponse[IO](headers = headers(Some("application/json"))))
        .map(value =>
          assertTrue(value.left.exists {
            case Http4sFailure.Response(violations) => Http4s.report(violations).contains("json")
            case _                                  => false
          })
        )
  )
