package io.taig.otter.http

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import fs2.Stream
import io.taig.otter.Json
import io.taig.otter.http.codec.Http4sPayload
import io.taig.otter.http.codec.Http4sResponseEncoder
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import org.http4s.Entity
import org.http4s.Header
import org.http4s.Headers
import org.http4s.HttpApp
import org.http4s.Request as Http4sRequest
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import zio.Scope
import zio.ZIO
import zio.test.*

import scala.compiletime.testing.typeCheckErrors

object Http4sPolicyDslTest extends ZIOSpecDefault:
  private def answer(code: Int, message: String): Response.Schema[Body.Whole[Json.Node], Failure, String] =
    response(Status(code))(body.json(payload.string)).dimap[Failure, String](_ => message)(identity)

  private val policy = errorPolicy.from(answer(500, "unexpected"))(
    validation = answer(422, "validation"),
    status = answer(504, "status"),
    syntax = answer(401, "syntax"),
    envelope = answer(400, "envelope"),
    encoding = answer(503, "encoding"),
    entityRead = answer(502, "entityRead"),
    contentType = answer(415, "contentType")
  )

  private val expected = List(
    (Failure.Category.Envelope, 400, "envelope"),
    (Failure.Category.Syntax, 401, "syntax"),
    (Failure.Category.ContentType, 415, "contentType"),
    (Failure.Category.Validation, 422, "validation"),
    (Failure.Category.EntityRead, 502, "entityRead"),
    (Failure.Category.Encoding, 503, "encoding"),
    (Failure.Category.Status, 504, "status"),
    (Failure.Category.Unexpected, 500, "unexpected")
  )

  private val defaults = List(
    Failure.Category.Envelope -> 400,
    Failure.Category.Syntax -> 400,
    Failure.Category.ContentType -> 415,
    Failure.Category.Validation -> 422,
    Failure.Category.EntityRead -> 500,
    Failure.Category.Encoding -> 500,
    Failure.Category.Status -> 500,
    Failure.Category.Unexpected -> 500
  )

  private val encoder = Http4sResponseEncoder[cats.effect.IO, io.taig.otter.Json.Node](Http4sCirce.Payload)
  private val cause = new IllegalStateException("private diagnostic")

  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  private def crash[A]: A = throw cause

  private def app[A](route: Route[IO, Http4sPayload.Supported[Json.Node], A, Unit]): HttpApp[IO] =
    Http4s.app[IO](Api(policy, unroutedPolicy.default), route)(Http4sCirce.Payload)

  private val get = Http4sRequest[IO](uri = uri"http://test")
  private def post(value: String, contentType: String): Http4sRequest[IO] = Http4sRequest[IO](
    uri = get.uri,
    method = org.http4s.Method.POST,
    headers = Headers(Header.Raw(CIString("Content-Type"), contentType)),
    entity = Entity.strict(ByteVector.encodeUtf8(value).getOrElse(ByteVector.empty))
  )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sPolicyDslTest")(
    test("every named category writes its own status and body regardless of argument order"):
      ZIO.fromFuture(_ =>
        expected
          .traverse { (category, code, message) =>
            encoder
              .encode(policy.responses, Failure(category))
              .map(
                _.exists(wire =>
                  wire.status == Status(code) && wire.body.exists(_._2.decodeUtf8 == Right(s"\"$message\""))
                )
              )
          }
          .map(results => assertTrue(results.forall(identity)))
          .unsafeToFuture()
      )
    ,
    test("an empty policy uses the documented bodyless default for every category"):
      val policy = errorPolicy()
      ZIO.fromFuture(_ =>
        defaults
          .traverse { (category, code) =>
            encoder
              .encode(policy.responses, Failure(category))
              .map(_.exists(wire => wire.status == Status(code) && wire.body.isEmpty))
          }
          .map(results => assertTrue(results.forall(identity)))
          .unsafeToFuture()
      )
    ,
    test("omitted categories keep bodyless defaults and only the named response changes"):
      val partial = errorPolicy(unexpected = answer(503, "local"))
      ZIO.fromFuture(_ =>
        defaults
          .traverse { (category, code) =>
            encoder
              .encode(partial.responses, Failure(category))
              .map(
                _.exists(wire =>
                  if category == Failure.Category.Unexpected then
                    wire.status == Status(503) && wire.body.exists(_._2.decodeUtf8 == Right("\"local\""))
                  else wire.status == Status(code) && wire.body.isEmpty
                )
              )
          }
          .map(results => assertTrue(results.forall(identity)))
          .unsafeToFuture()
      )
    ,
    test("a custom baseline fills every omitted category and an empty override inherits everything"):
      val baseline = errorPolicy.from(answer(502, "baseline"))()
      val inherited = errorOverrides()(baseline)
      ZIO.fromFuture(_ =>
        Failure.Category.values.toList
          .traverse(category =>
            encoder
              .encode(inherited.responses, Failure(category))
              .map(
                _.exists(wire =>
                  wire.status == Status(502) && wire.body.exists(_._2.decodeUtf8 == Right("\"baseline\""))
                )
              )
          )
          .map(results => assertTrue(inherited == baseline, results.forall(identity)))
          .unsafeToFuture()
      )
    ,
    test("an endpoint override replaces one category and inherits every other response"):
      val domain = endpoint(request(method.get, __), response(status.noContent))
      val declared = domain.withErrors(errorOverrides(syntax = Some(answer(409, "local"))))
      val composed = declared.compose(policy)
      ZIO.fromFuture(_ =>
        expected
          .traverse { (category, code, message) =>
            val (expectedCode, expectedMessage) =
              if category == Failure.Category.Syntax then (409, "local") else (code, message)
            encoder
              .encode(composed.errors.responses, Failure(category))
              .map(
                _.exists(wire =>
                  wire.status == Status(expectedCode) && wire.body
                    .exists(_._2.decodeUtf8 == Right(s"\"$expectedMessage\""))
                )
              )
          }
          .map(results => assertTrue(results.forall(identity)))
          .unsafeToFuture()
      )
    ,
    test("actual interpreter failures select all eight named responses"):
      val plain = endpoint(request(method.get, __), response(status.noContent))
      val json = endpoint(request(method.post, __)(body.json(payload.int)), response(status.noContent))
      val readJson = app(Route(json, (_: Int) => IO.unit))
      val path = endpoint(request(method.get, __ / segment("id", int)), response(status.noContent))
      val binary = endpoint(request(method.post, __)(body.binary), response(status.noContent))
      val invalid = endpoint(request(method.get, __), response(Status(-1)))
      val broken = endpoint(request(method.get, __), response(status.noContent).dimap[Unit, Unit](_ => crash)(identity))
      val cases = List(
        (app(Route(path, (_: Int) => IO.unit)), get.withUri(uri"http://test/invalid"), 400, "envelope"),
        (readJson, post("not json", "application/json"), 401, "syntax"),
        (readJson, post("1", "text/plain"), 415, "contentType"),
        (readJson, post("\"text\"", "application/json"), 422, "validation"),
        (
          app(Route(binary, (_: ByteVector) => IO.unit)),
          Http4sRequest[IO](
            method = org.http4s.Method.POST,
            uri = get.uri,
            entity = Entity.Streamed(Stream.raiseError[IO](cause), None)
          ),
          502,
          "entityRead"
        ),
        (app(Route(broken, (_: Unit) => IO.unit)), get, 503, "encoding"),
        (app(Route(invalid, (_: Unit) => IO.unit)), get, 504, "status"),
        (app(Route(plain, (_: Unit) => IO.raiseError[Unit](cause))), get, 500, "unexpected")
      )
      ZIO
        .fromFuture(_ =>
          cases
            .traverse { (app, request, code, message) =>
              app
                .run(request)
                .flatMap(response =>
                  Http4sEnvelope
                    .toBytes(response.entity)
                    .map(bytes => response.status.code == code && bytes.decodeUtf8 == Right(s"\"$message\""))
                )
            }
            .unsafeToFuture()
        )
        .map(results => assertTrue(results.forall(identity)))
    ,
    test("DSL inference preserves baseline types and accumulates replacement requirements"):
      val errors = typeCheckErrors("""
        import cats.syntax.all.*
        import io.taig.otter.Json
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.http.fixture.payload
        import scodec.bits.ByteVector
        final class TypeOf[A](value: A):
          def is[B](using A =:= B): Boolean = true
        def inferred[A](value: A): TypeOf[A] = new TypeOf(value)
        val json: Response.Schema[Body.Whole[Json.Node], Failure, String] = response(Status(503))(body.json(payload.string)).dimap[Failure, String](_ => "error")(identity)
        val binary: Response.Schema[Body.Opaque, Failure, ByteVector] = response(Status(502))(body.binary).dimap[Failure, ByteVector](_ => ByteVector.empty)(identity)
        val defaults = errorPolicy()
        inferred(defaults).is[ErrorPolicy[[w, r] =>> Nothing, Status]]
        val baseline = errorPolicy.from(json)()
        inferred(baseline).is[ErrorPolicy[Body.Whole[Json.Node], String]]
        val partial = errorPolicy(unexpected = json)
        inferred(partial).is[ErrorPolicy[Body.Whole[Json.Node], Status | String]]
        val mixed = errorPolicy.from(json)(encoding = binary)
        val supported: ErrorPolicy[Body.Or[Body.Whole[Json.Node], Body.Opaque], String | ByteVector] = mixed
        val multiple = errorPolicy(encoding = binary, unexpected = json)
        inferred(multiple).is[ErrorPolicy[Body.Or[Body.Whole[Json.Node], Body.Opaque], Status | String | ByteVector]]
        val overrides = errorOverrides(encoding = Some(binary), unexpected = Some(json))
        inferred(overrides).is[ErrorOverrides[Body.Or[Body.Whole[Json.Node], Body.Opaque], String | ByteVector]]
        val empty = errorOverrides()
        inferred(empty).is[ErrorOverrides[[w, r] =>> Nothing, Nothing]]
        val unrouted = unroutedPolicy()
        inferred(unrouted).is[UnroutedPolicy[[w, r] =>> Nothing]]
      """)
      assertTrue(errors.isEmpty)
  )
