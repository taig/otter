package io.taig.otter.http

import cats.data.Chain
import cats.data.Validated
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import fs2.Stream
import io.taig.otter.Json
import io.taig.otter.Violations
import io.taig.otter.http.codec.Http4sPayload
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.fixture.dsl
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import org.http4s.Entity
import org.http4s.Request as Http4sRequest
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.implicits.*
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

object Http4sErrorPolicyTest extends ZIOSpecDefault:
  private val base: Uri = uri"http://otter.test"
  private val domain = endpoint(request(method.get, __), response(status.noContent))
  private val composed = ErrorPolicy.default(domain)
  private val cause = new IllegalStateException("private diagnostic")

  /** Injects a synchronous exception to verify the interpreter's effect boundary. */
  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  private def crash[A]: A = throw cause

  /** Reads every JSON schema and writes none of them, which is the one shortfall an interpreter can still have. */
  private val refusingPayload: Http4sPayload.Of[Json.Node] =
    Http4sPayload(Http4sCirce.Alphabet)(new Http4sPayload.Codec[Json.Node]:
      override def decode[R](schema: Json.Node[Nothing, R], bytes: ByteVector): Validated[Violations, R] =
        Http4sErrorPolicyTest.crash
      override def encode[W](schema: Json.Node[W, Any], value: W): Either[String, ByteVector] = Left("Cannot encode"))

  private def run[A](value: IO[A]): Task[A] = ZIO.fromFuture(_ => value.unsafeToFuture())

  private def failed(handler: Unit => IO[Unit]): IO[(Int, ByteVector, List[Http4sObservation.Event])] =
    for
      events <- IO.ref(List.empty[Http4sObservation.Event])
      response <- Http4s
        .routes[IO](
          Route(composed, handler)
        )(Http4sPayload.Empty, observation => events.update(_ :+ observation.event))
        .orNotFound
        .run(Http4sRequest[IO](uri = base))
      bytes <- Http4sEnvelope.toBytes(response.entity)
      seen <- events.get
    yield (response.status.code, bytes, seen)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sErrorPolicyTest")(
    test("request failures retain categories, parser causes, and accumulated violations"):
      val schema = request(method.post, __ / segment("id", int))
        .queries(query("page", int).toRecord)(body.json(payload.int))
      val decoder = Http4sRequestDecoder(Http4sCirce.Payload)
      def decode(content: String, media: MediaType, page: String = "1") =
        decoder
          .decodeDetailed(
            schema,
            Http4sWire.Request(
              Vector("1"),
              Chain.one("page" -> Some(page)),
              Chain.empty,
              (Some(media), ByteVector.encodeUtf8(content).getOrElse(ByteVector.empty))
            )
          )
          .swap
          .toOption
      val syntax = decode("not json", dsl.mediaType.json)
      val validation = decode("\"text\"", dsl.mediaType.json)
      val unsupported = decode("1", dsl.mediaType.text)
      val mixed = decode("not json", dsl.mediaType.json, "invalid")
      assertTrue(
        syntax.exists(failure => failure.category == Failure.Category.Syntax && failure.cause.nonEmpty),
        validation.exists(_.category == Failure.Category.Validation),
        unsupported.exists(_.category == Failure.Category.ContentType),
        mixed.exists(failure =>
          failure.category == Failure.Category.Envelope &&
            Http4s.report(failure.violations).contains("$.query.page") &&
            Http4s.report(failure.violations).contains("$.body")
        )
      )
    ,
    test("an eligible payload failure takes priority over a different alternative's content type"):
      val schema = request(method.post, __)(body.json(payload.int) :+ body.binary(dsl.mediaType.pdf))
      val result = Http4sRequestDecoder(Http4sCirce.Payload).decodeDetailed(
        schema,
        Http4sWire.Request(
          Vector.empty,
          Chain.empty,
          Chain.empty,
          (Some(dsl.mediaType.json), ByteVector.encodeUtf8("not json").getOrElse(ByteVector.empty))
        )
      )
      assertTrue(result.swap.toOption.exists(_.category == Failure.Category.Syntax))
    ,
    test("unexpected effect failures use a bodyless 500 and retain their cause"):
      run(failed(_ => IO.raiseError(cause))).map { (status, bytes, events) =>
        assertTrue(
          status == 500,
          bytes.isEmpty,
          events == List(Http4sObservation.Event.Failed(Failure(Failure.Category.Unexpected, cause = Some(cause))))
        )
      }
    ,
    test("synchronous handler failures use the same policy"):
      run(failed(_ => Http4sErrorPolicyTest.crash)).map((status, _, events) =>
        assertTrue(status == 500, events.size == 1)
      )
    ,
    test("a composed client decodes a framework error as a typed value"):
      val client = Client.fromHttpApp(
        Http4s
          .routes[IO](
            Route(composed, (_: Unit) => IO.raiseError[Unit](cause))
          )(Http4sPayload.Empty)
          .orNotFound
      )
      run(Http4s.client[IO, Unit, Either[Status, Unit]](composed.effective)(Http4sPayload.Empty, base, client)(()))
        .map(value => assertTrue(value == Left(Status(500))))
    ,
    test("a composed client keeps a domain answer in the right branch"):
      val client = Client.fromHttpApp(
        Http4s
          .routes[IO](
            Route(composed, (_: Unit) => IO.unit)
          )(Http4sPayload.Empty)
          .orNotFound
      )
      run(Http4s.client[IO, Unit, Either[Status, Unit]](composed.effective)(Http4sPayload.Empty, base, client)(()))
        .map(value => assertTrue(value == Right(())))
    ,
    test("overlapping wire responses retain domain priority"):
      val value: ComposedEndpoint[[w, r] =>> Nothing, Unit, Unit, Unit, Unit, Status] =
        ErrorPolicy.default(endpoint(request(method.get, __), response(Status(500))))
      val client =
        Client.fromHttpApp(Http4s.routes[IO](Route(value, (_: Unit) => IO.unit))(Http4sPayload.Empty).orNotFound)
      run(Http4s.client[IO, Unit, Either[Status, Unit]](value.effective)(Http4sPayload.Empty, base, client)(()))
        .map(value => assertTrue(value == Right(())))
    ,
    test("a declared JSON error supports a custom status, headers, and typed client value"):
      val error = response(Status(503))
        .headers(header("Retry-After", int).toRecord)(body.json(payload.string))
        .dimap[Failure, String](_ => (5, "unavailable"))(_._2)
      val policy: ErrorPolicy[Body.Whole[Json.Node], Status | String] = ErrorPolicy.default.copy(unexpected = error)
      val value = policy(domain)
      val app = Http4s.routes[IO](Route(value, (_: Unit) => IO.raiseError[Unit](cause)))(Http4sCirce.Payload).orNotFound
      run(
        for
          response <- app.run(Http4sRequest[IO](uri = base))
          bytes <- Http4sEnvelope.toBytes(response.entity)
          decoded <- Http4s
            .client[IO, Unit, Either[Status | String, Unit]](
              value.effective
            )(Http4sCirce.Payload, base, Client.fromHttpApp(app))(())
        yield assertTrue(
          response.status.code == 503,
          response.headers.headers.exists(header => header.name.toString == "Retry-After" && header.value == "5"),
          bytes.decodeUtf8 == Right("\"unavailable\""),
          decoded == Left("unavailable")
        )
      )
    ,
    test("entity read failures are observed separately and do not call the handler"):
      val value = ErrorPolicy.default(endpoint(request(method.post, __)(body.binary), response(status.noContent)))
      run(
        for
          events <- IO.ref(List.empty[Http4sObservation.Event])
          called <- IO.ref(false)
          response <- Http4s
            .routes[IO](
              Route(value, (_: ByteVector) => called.set(true))
            )(Http4sPayload.Empty, observation => events.update(_ :+ observation.event))
            .orNotFound
            .run(
              Http4sRequest[IO](
                method = org.http4s.Method.POST,
                uri = base,
                entity = Entity.Streamed(Stream.raiseError[IO](cause), None)
              )
            )
          seen <- events.get
          invoked <- called.get
        yield assertTrue(
          response.status.code == 500,
          !invoked,
          seen == List(Http4sObservation.Event.Failed(Failure(Failure.Category.EntityRead, cause = Some(cause))))
        )
      )
    ,
    test("an invalid domain status uses the declared status-error response"):
      val value: ComposedEndpoint[[w, r] =>> Nothing, Unit, Unit, Unit, Unit, Status] =
        ErrorPolicy.default(endpoint(request(method.get, __), response(Status(-1))))
      run(
        Http4s
          .routes[IO](Route(value, (_: Unit) => IO.unit))(Http4sPayload.Empty)
          .orNotFound
          .run(Http4sRequest[IO](uri = base))
      ).map(response => assertTrue(response.status.code == 500))
    ,
    test("an error mapping failure is observed once and never recursively handled"):
      val broken = response(Status(503)).dimap[Failure, Status](_ => Http4sErrorPolicyTest.crash)(_ => Status(503))
      val value = ErrorPolicy.default.copy(unexpected = broken)(domain)
      run(for
        events <- IO.ref(List.empty[Http4sObservation.Event])
        response <- Http4s
          .routes[IO](
            Route(value, (_: Unit) => IO.raiseError[Unit](new RuntimeException("handler")))
          )(Http4sPayload.Empty, observation => events.update(_ :+ observation.event))
          .orNotFound
          .run(Http4sRequest[IO](uri = base))
          .attempt
        seen <- events.get
      yield assertTrue(response == Left(cause), seen.size == 2, seen.lastOption.contains(Http4sObservation.Event.ErrorResponseFailed(cause))))
    ,
    test("cancellation remains cancellation and is observed without a response"):
      run(for
        started <- IO.deferred[Unit]
        events <- IO.ref(List.empty[Http4sObservation.Event])
        fiber <- Http4s
          .routes[IO](
            Route(composed, (_: Unit) => started.complete(()) *> IO.never[Unit])
          )(Http4sPayload.Empty, observation => events.update(_ :+ observation.event))
          .orNotFound
          .run(Http4sRequest[IO](uri = base))
          .start
        _ <- started.get
        _ <- fiber.cancel
        outcome <- fiber.join
        seen <- events.get
      yield assertTrue(outcome.isCanceled, seen == List(Http4sObservation.Event.Cancelled)))
    ,
    test("response encoding exceptions have their own category"):
      val answer =
        response(status.ok)(body.json(payload.string)).dimap[Unit, String](_ => Http4sErrorPolicyTest.crash)(identity)
      val value = ErrorPolicy.default(endpoint(request(method.get, __), answer))
      run(for
        events <- IO.ref(List.empty[Http4sObservation.Event])
        response <- Http4s
          .routes[IO](
            Route(value, (_: Unit) => IO.unit)
          )(Http4sCirce.Payload, observation => events.update(_ :+ observation.event))
          .orNotFound
          .run(Http4sRequest[IO](uri = base))
        seen <- events.get
      yield assertTrue(response.status.code == 500, seen == List(Http4sObservation.Event.Failed(Failure(Failure.Category.Encoding, cause = Some(cause))))))
    ,
    // An alphabet the registry does not cover was once reported here too, as a separate internal category. It is not a
    // case any more: `Http4sPayload.Alphabet` chooses between two alphabets rather than answering whether a payload is
    // one it knows, so a registry that falls short is rejected where the routes are built. `LibraryShortfallTest` and
    // `Http4sFs2DataTest` are where that is now asserted, and they assert it of the compiler.
    test("a codec that recognizes a body and cannot write it is an encoding failure"):
      val value = ErrorPolicy.default(endpoint(request(method.get, __), response(status.ok)(body.json(payload.string))))
      val answer =
        for
          events <- IO.ref(List.empty[Http4sObservation.Event])
          response <- Http4s
            .routes[IO](
              Route(value, (_: Unit) => IO.pure("value"))
            )(refusingPayload, observation => events.update(_ :+ observation.event))
            .orNotFound
            .run(Http4sRequest[IO](uri = base))
          seen <- events.get
        yield (response.status.code, seen)
      run(answer).map { (status, events) =>
        def category(events: List[Http4sObservation.Event]): Option[Failure.Category] = events.headOption.collect {
          case Http4sObservation.Event.Failed(failure) => failure.category
        }
        assertTrue(status == 500, category(events).contains(Failure.Category.Encoding))
      }
  )
