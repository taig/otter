package io.taig.otter.http

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import io.taig.otter.http.Sized
import io.taig.otter.http.codec.Http4sInterpreter
import io.taig.otter.http.codec.Http4sPayload
import io.taig.otter.http.codec.Http4sStreams
import io.taig.otter.http.fixture.dsl.*
import org.http4s.Entity
import org.http4s.Header
import org.http4s.Headers as HttpHeaders
import org.http4s.HttpApp
import org.http4s.Request as HttpRequest
import org.http4s.client.Client
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

object Http4sSizedBodyTest extends ZIOSpecDefault:
  type Flow[+A] = Stream[IO, A]

  private val base = uri"http://sized.test"
  private val interpreter = Http4sPayload.Empty.withStreams(Http4sStreams.Raw)
  private val upload = endpoint(request(method.post, __)(body.streamed[Flow].sized()), response(status.noContent))

  private def run[A](io: IO[A]): Task[A] = ZIO.fromFuture(_ => io.unsafeToFuture())

  private def post(bytes: ByteVector, headers: List[(String, String)]): HttpRequest[IO] =
    HttpRequest[IO](
      method = org.http4s.Method.POST,
      uri = base,
      headers = HttpHeaders(
        (("Content-Type", "application/octet-stream") :: headers).map((name, value) =>
          Header.Raw(CIString(name), value)
        )
      ),
      entity = Entity.Streamed(Stream.chunk(fs2.Chunk.byteVector(bytes)).covary[IO], None)
    )

  /** What a request was answered with, what its handler was given, whether its body was pulled at all, and the failure
    * categories observed.
    */
  final case class Outcome(
      status: Int,
      received: Option[(Long, Long)],
      pulled: Boolean,
      observed: List[Failure.Category]
  )

  private def serve(
      request: HttpRequest[IO],
      interpreter: Http4sInterpreter.Of[Nothing, Body.Raw] = interpreter
  ): IO[Outcome] =
    for
      events <- IO.ref(List.empty[Http4sObservation.Event])
      received <- IO.ref(Option.empty[(Long, Long)])
      pulled <- IO.ref(false)
      app = Http4s.app[IO](
        Route(
          upload,
          (sized: Sized[Flow]) => sized.bytes.compile.count.flatMap(count => received.set(Some((sized.length, count))))
        )
      )(interpreter, observation => events.update(_ :+ observation.event))
      response <- app.run(request.withBodyStream(Stream.exec(pulled.set(true)) ++ request.body))
      handed <- received.get
      read <- pulled.get
      seen <- events.get
    yield Outcome(
      response.status.code,
      handed,
      read,
      seen.collect { case Http4sObservation.Event.Failed(failure) => failure.category }
    )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sSizedBodyTest")(
    test("the handler receives the declared length together with the stream"):
      val bytes = ByteVector.fill(64)(7)
      run(serve(post(bytes, List("Content-Length" -> "64")))).map: outcome =>
        assertTrue(outcome == Outcome(204, Some((64L, 64L)), pulled = true, Nil))
    ,
    test("the declared length is handed over as declared, whatever the stream then yields"):
      run(serve(post(ByteVector.fill(3)(7), List("Content-Length" -> "5")))).map: outcome =>
        assertTrue(outcome.status == 204, outcome.received.contains((5L, 3L)))
    ,
    test("a body the backend already holds is handed over with its length"):
      val bytes = ByteVector.fill(4)(7)
      run(serve(post(bytes, List("Content-Length" -> "4")).withEntity(Entity.strict(bytes)))).map: outcome =>
        assertTrue(outcome.status == 204, outcome.received.contains((4L, 4L)))
    ,
    test("a request without a Content-Length is answered 411 before its body is read"):
      run(serve(post(ByteVector.fill(4)(7), Nil))).map: outcome =>
        assertTrue(outcome == Outcome(411, None, pulled = false, List(Failure.Category.LengthRequired)))
    ,
    test("a chunked request is answered 411 before its body is read, whatever Content-Length it carries"):
      run(serve(post(ByteVector.fill(4)(7), List("Transfer-Encoding" -> "chunked", "Content-Length" -> "4")))).map:
        outcome => assertTrue(outcome == Outcome(411, None, pulled = false, List(Failure.Category.LengthRequired)))
    ,
    test("a declared length past maxBufferedBytes reaches its handler whole"):
      val bytes = ByteVector.fill(1024)(7)
      run(serve(post(bytes, List("Content-Length" -> "1024")), interpreter.withMaxBufferedBytes(16))).map: outcome =>
        assertTrue(outcome == Outcome(204, Some((1024L, 1024L)), pulled = true, Nil))
    ,
    test("the client sends the length as Content-Length"):
      run(for
        headers <- IO.ref(Option.empty[String])
        received <- IO.ref(Option.empty[(Long, Long)])
        served = Http4s.app[IO](
          Route(
            upload,
            (sized: Sized[Flow]) =>
              sized.bytes.compile.count.flatMap(count => received.set(Some((sized.length, count))))
          )
        )(interpreter)
        app = HttpApp[IO](request =>
          headers.set(request.headers.get(CIString("Content-Length")).map(_.head.value)) *> served.run(request)
        )
        _ <- Http4s.client(interpreter, base, Client.fromHttpApp(app))(upload)(
          Sized(3, Stream.emits(List[Byte](1, 2, 3)))
        )
        header <- headers.get
        handed <- received.get
      yield assertTrue(header.contains("3"), handed.contains((3L, 3L))))
    ,
    test("a sized response reaches the client with its length"):
      val sized = body.streamed[Flow].sized()
      val download = endpoint(request(method.get, __), response(status.ok)(sized))
      val app = Http4s.app[IO](Route(download, (_: Unit) => IO.pure(Sized(2, Stream.emits(List[Byte](4, 5))))))(
        interpreter
      )
      run(
        Http4s
          .client(interpreter, base, Client.fromHttpApp(app))
          .resource(download)(())
          .use(sized => sized.bytes.compile.toList.map((sized.length, _)))
      ).map(result => assertTrue(result == (2L, List[Byte](4, 5))))
  )
