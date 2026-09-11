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

object Http4sEmptyBodyTest extends ZIOSpecDefault:
  private val Base: Uri = uri"http://otter.test"
  private val Png: MediaType = MediaType("image", "png")
  private val png: Body.Of[Body.Opaque, ByteVector] = body.binary(Png)
  private val text: Body.Of[Body.Opaque, ByteVector] = body.binary(MediaType.Text)
  private val EmptyEntities: List[(String, Entity[IO])] = List(
    "empty" -> Entity.empty,
    "strict" -> Entity.Strict(ByteVector.empty),
    "streamed" -> Entity.Streamed(Stream.empty, None)
  )

  private val upload: Endpoint[Option[ByteVector], Unit] = endpoint(
    request(method.post, __)(body.optional(png)),
    result(code.noContent).toUnion
  )

  private val download: Endpoint[Unit, ByteVector] = endpoint(
    request(method.get, __),
    result(code.ok)(png).toUnion
  )

  private val alternatives: Endpoint[Unit, Either[ByteVector, ByteVector]] = endpoint(
    request(method.get, __),
    result(code.ok)(png) :+ result(code.ok)(text)
  )

  private def headers(contentType: Option[String]): Http4sHeaders =
    Http4sHeaders(contentType.toList.map(value => Http4sHeader.Raw(CIString("Content-Type"), value)))

  private def received[A](endpoint: Endpoint[A, Unit], request: Http4sRequest[IO]): Task[(Int, Option[A])] =
    ZIO.fromFuture: _ =>
      IO.ref(Option.empty[A])
        .flatMap: ref =>
          Http4s
            .routes[IO](Http4sCirce.Payload)(Route(endpoint, (value: A) => ref.set(Some(value))))
            .orNotFound
            .run(request)
            .flatMap(response => ref.get.map((response.status.code, _)))
        .unsafeToFuture()

  private def decoded[A](endpoint: Endpoint[Unit, A], response: Http4sResponse[IO]): Task[Either[Throwable, A]] =
    ZIO.fromFuture: _ =>
      val client = Http4sClient.fromHttpApp[IO](org.http4s.HttpApp[IO](_ => IO.pure(response)))

      Http4s.client[IO, Unit, A](Http4sCirce.Payload, Base, client)(endpoint)(()).attempt.unsafeToFuture()

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sEmptyBodyTest")(
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

            received(upload, request).map((code, value) => assertTrue(code == 422, value.isEmpty))
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

                Http4s.client[IO, Option[ByteVector], Unit](Http4sCirce.Payload, Base, client)(upload)(value) *> ref.get
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
        result(code.ok)(body.json(api.settings)).toUnion
      )

      decoded(endpoint, Http4sResponse[IO](headers = headers(Some("application/json"))))
        .map(value =>
          assertTrue(value.left.exists {
            case Http4sFailure.Response(violations) => Http4s.report(violations).contains("json")
            case _                                  => false
          })
        )
  )
