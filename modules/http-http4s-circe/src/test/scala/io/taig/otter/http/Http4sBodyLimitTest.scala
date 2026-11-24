package io.taig.otter.http

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import fs2.Stream
import io.taig.otter.http.codec.Http4sPayload
import io.taig.otter.http.codec.Http4sStreams
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload as json
import org.http4s.Entity
import org.http4s.Header
import org.http4s.Headers as HttpHeaders
import org.http4s.HttpApp
import org.http4s.Request as HttpRequest
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

object Http4sBodyLimitTest extends ZIOSpecDefault:
  type Flow[+A] = Stream[IO, A]

  private val limit = 16L
  private val payload = Http4sCirce.Payload.withMaxBufferedBytes(limit)
  private val document = endpoint(request(method.post, __)(body.json(json.string)), response(status.noContent))

  private def run[A](io: IO[A]): Task[A] = ZIO.fromFuture(_ => io.unsafeToFuture())

  /** A JSON string of exactly `size` bytes, quotes included. */
  private def text(size: Int): ByteVector = ByteVector.encodeUtf8("\"" + ("a" * (size - 2)) + "\"").toOption.get

  private def post(bytes: ByteVector, length: Boolean, media: String = "application/json"): HttpRequest[IO] =
    HttpRequest[IO](
      method = org.http4s.Method.POST,
      uri = uri"http://limit.test",
      headers = HttpHeaders(
        Header.Raw(CIString("Content-Type"), media) ::
          (if length then List(Header.Raw(CIString("Content-Length"), bytes.size.toString)) else Nil)
      ),
      entity = Entity.Streamed(Stream.chunk(fs2.Chunk.byteVector(bytes)).covary[IO], None)
    )

  /** What a request was answered with, whether a handler ran, and the failure categories observed. */
  final case class Outcome(status: Int, invoked: Boolean, observed: List[Failure.Category])

  private def serve(app: (IO[Unit], Http4sObservation[IO] => IO[Unit]) => HttpApp[IO])(
      request: HttpRequest[IO]
  ): IO[Outcome] =
    for
      events <- IO.ref(List.empty[Http4sObservation.Event])
      called <- IO.ref(false)
      response <- app(called.set(true), observation => events.update(_ :+ observation.event)).run(request)
      invoked <- called.get
      seen <- events.get
    yield Outcome(
      response.status.code,
      invoked,
      seen.collect { case Http4sObservation.Event.Failed(failure) => failure.category }
    )

  private val served =
    serve((handled, observe) => Http4s.app[IO](Route(document, (_: String) => handled))(payload, observe))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sBodyLimitTest")(
    test("a whole body past the maximum is answered 413 without reaching the handler"):
      run(served(post(text(limit.toInt + 1), length = true))).map: outcome =>
        assertTrue(outcome == Outcome(413, invoked = false, List(Failure.Category.ContentTooLarge)))
    ,
    test("a whole body is read up to and including the maximum, whether or not it declares its length"):
      val cases = for
        size <- List(limit.toInt - 1, limit.toInt, limit.toInt + 1)
        length <- List(true, false)
      yield (size, length)
      run(cases.traverse((size, length) => served(post(text(size), length)).map((size, length, _)))).map: outcomes =>
        assertTrue(
          outcomes.map((size, length, outcome) => (size, length, outcome.status, outcome.invoked)) == List(
            (15, true, 204, true),
            (15, false, 204, true),
            (16, true, 204, true),
            (16, false, 204, true),
            (17, true, 413, false),
            (17, false, 413, false)
          )
        )
    ,
    test("a body already held in memory is bounded the same way"):
      def held(size: Int) = post(text(size), length = true).withEntity(Entity.strict(text(size)))
      run((served(held(limit.toInt)), served(held(limit.toInt + 1))).tupled).map: (at, past) =>
        assertTrue(
          at == Outcome(204, invoked = true, Nil),
          past == Outcome(413, invoked = false, List(Failure.Category.ContentTooLarge))
        )
    ,
    test("a chunked request's Content-Length is not trusted over the bytes that arrive"):
      val request = post(text(limit.toInt), length = false)
      val chunked = request.putHeaders(
        Header.Raw(CIString("Transfer-Encoding"), "chunked"),
        Header.Raw(CIString("Content-Length"), (limit * 2).toString)
      )
      run(served(chunked)).map(outcome => assertTrue(outcome == Outcome(204, invoked = true, Nil)))
    ,
    test("a Content-Length past the maximum is answered 413 before any of the body is read"):
      run(for
        pulled <- IO.ref(false)
        request = post(text(limit.toInt + 1), length = true)
        outcome <- served(request.withBodyStream(Stream.exec(pulled.set(true)) ++ request.body))
        read <- pulled.get
      yield assertTrue(outcome == Outcome(413, invoked = false, List(Failure.Category.ContentTooLarge)), !read))
    ,
    test("a raw byte stream past the maximum reaches its handler whole"):
      val raw = endpoint(request(method.post, __)(body.streamed[Flow].raw()), response(status.noContent))
      val interpreter = Http4sPayload.Empty.withStreams(Http4sStreams.Raw).withMaxBufferedBytes(limit)
      val bytes = ByteVector.fill(limit * 64)(1)
      run(for
        received <- IO.ref(0L)
        outcome <- serve((handled, observe) =>
          Http4s.app[IO](
            Route(raw, (values: Flow[Byte]) => values.compile.count.flatMap(received.set) *> handled)
          )(interpreter, observe)
        )(post(bytes, length = true, "application/octet-stream"))
        count <- received.get
      yield assertTrue(outcome == Outcome(204, invoked = true, Nil), count == bytes.size))
    ,
    test("a framed stream is bounded per frame and not by the maximum"):
      val rows = endpoint(request(method.post, __)(body.ndjson[Flow](json.int)), response(status.noContent))
      val interpreter =
        Http4sCirce.Payload.withStreams(Http4sCirce.streams(maxFrameBytes = 4)).withMaxBufferedBytes(limit)
      def send(lines: List[String]) = serve((handled, observe) =>
        Http4s.app[IO](Route(rows, (values: Flow[Int]) => values.compile.drain *> handled))(interpreter, observe)
      )(post(ByteVector.encodeUtf8(lines.map(_ + "\n").mkString).toOption.get, length = true, "application/x-ndjson"))
      run((send(List.fill(limit.toInt)("1234")), send(List("12345"))).tupled).map: (within, past) =>
        assertTrue(
          within == Outcome(204, invoked = true, Nil),
          past.status == 400,
          past.observed == List(Failure.Category.Syntax)
        )
  )
