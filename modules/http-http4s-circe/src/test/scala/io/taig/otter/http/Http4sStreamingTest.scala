package io.taig.otter.http

import cats.effect.IO
import cats.effect.Resource
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import fs2.Stream
import io.taig.otter.http.codec.Http4sStreams
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload as json
import org.http4s.Entity
import org.http4s.Header
import org.http4s.Headers as HttpHeaders
import org.http4s.Request as HttpRequest
import org.http4s.Response as HttpResponse
import org.http4s.client.Client
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

import scala.compiletime.testing.typeChecks

object Http4sStreamingTest extends ZIOSpecDefault:
  type Flow[+A] = Stream[IO, A]
  val payload = Http4sCirce.Payload.withStreams(Http4sCirce.Streams.orElse(Http4sStreams.Raw))
  final case class Batch(values: Flow[Int])
  val rows = body.ndjson[Http4sStreamingTest.Flow](json.int)
  val download = endpoint(request(method.get, __ / "rows"), response(status.ok)(rows))
  val upload = endpoint(request(method.post, __ / "rows")(rows), response(status.ok)(body.json(json.int)))
  private val base = uri"http://stream.test"
  private def run[A](io: IO[A]): Task[A] = ZIO.fromFuture(_ => io.unsafeToFuture())
  private def bytes(value: String): ByteVector =
    ByteVector.view(value.getBytes(java.nio.charset.StandardCharsets.UTF_8))
  private def wireResponse(source: Stream[IO, Byte], media: String = "application/x-ndjson"): HttpResponse[IO] =
    HttpResponse[IO](headers = HttpHeaders(Header.Raw(CIString("Content-Type"), media)), entity = Entity.stream(source))
  private def transport(source: Stream[IO, Byte], release: IO[Unit] = IO.unit): Client[IO] =
    Client[IO](_ => Resource.make(IO.pure(wireResponse(source)))(_ => release))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sStreamingTest")(
    test("all framed modes stream requests and responses through the same declaration"):
      val frames = List(Frame.Lines, Frame.Events, Frame.Delimited("<end>"))
      run(frames.traverse: frame =>
        val rows =
          body.streamed[Http4sStreamingTest.Flow](io.taig.otter.http.fixture.dsl.mediaType.ndJson, frame, json.string)
        val echo = endpoint(request(method.post, __)(rows), response(status.ok)(rows))
        val app = Http4s.app[IO](Route(echo, (values: Stream[IO, String]) => IO.pure(values)))(payload)
        val call = Http4s.client(payload, base, Client.fromHttpApp(app)).resource(echo)
        List(List("one", "héllo", "three"), Nil).traverse(values =>
          call(Stream.emits(values)).use(_.compile.toList).map(_ == values)
        )).map(results => assertTrue(results.flatten.forall(identity)))
    ,
    test("element read and write types stay independent through the carrier"):
      val element = json.int.dimap[String, Long](_.toInt)(_.toLong)
      val rows = body.ndjson[Flow](element)
      val echo = endpoint(request(method.post, __)(rows), response(status.ok)(rows))
      val app = Http4s.app[IO](Route(echo, (values: Flow[Long]) => IO.pure(values.map(_.toString))))(payload)
      run(
        Http4s
          .client(payload, base, Client.fromHttpApp(app))
          .resource(echo)(Stream.emits(List("1", "2")))
          .use(_.compile.toList)
      ).map(result => assertTrue(result == List(1L, 2L)))
    ,
    test("raw byte streams preserve arbitrary binary values in both directions"):
      val rawPayload = io.taig.otter.http.codec.Http4sPayload.Empty.withStreams(Http4sStreams.Raw)
      val raw = body.streamed[Http4sStreamingTest.Flow].raw()
      val echo = endpoint(request(method.post, __)(raw), response(status.ok)(raw))
      val app = Http4s.app[IO](Route(echo, (values: Stream[IO, Byte]) => IO.pure(values)))(rawPayload)
      val call = Http4s.client(rawPayload, base, Client.fromHttpApp(app)).resource(echo)
      run(
        List(List[Byte](0, -1, 13, 10), Nil).traverse(values =>
          call(Stream.emits(values)).use(_.compile.toList).map(_ == values)
        )
      )
        .map(results => assertTrue(results.forall(identity)))
    ,
    test("streamed uploads retain the buffered client convenience API"):
      val app = Http4s.app[IO](Route(upload, (values: Stream[IO, Int]) => values.compile.count.map(_.toInt)))(payload)
      run(Http4s.client(payload, base, Client.fromHttpApp(app))(upload)(Stream.emits(List(1, 2, 3))))
        .map(count => assertTrue(count == 3))
    ,
    test("empty streams are present bodies and retain their content type"):
      val app = Http4s.app[IO](Route(download, (_: Unit) => IO.pure(Stream.empty[IO])))(payload)
      run(for
        wire <- app(HttpRequest[IO](uri = base / "rows"))
        output <- wire.body.compile.toVector
        decoded <- Http4s.client(payload, base, Client.fromHttpApp(app)).resource(download)(()).use(_.compile.toList)
      yield assertTrue(output.isEmpty, decoded.isEmpty, Http4sEnvelope.toMediaType(wire.headers).contains(io.taig.otter.http.fixture.dsl.mediaType.ndJson)))
    ,
    test("resource acquisition is lazy and an infinite response is consumed incrementally"):
      run(for
        reads <- IO.ref(0)
        released <- IO.ref(0)
        finalized <- IO.ref(0)
        source = Stream
          .repeatEval(reads.update(_ + 1).as(bytes("1\n")))
          .flatMap(b => Stream.chunk(fs2.Chunk.byteVector(b)))
          .onFinalize(finalized.update(_ + 1))
        call = Http4s.client(payload, base, transport(source, released.update(_ + 1))).resource(download)
        before <- reads.get
        result <- call(()).use(_.take(3).compile.toList)
        pulled <- reads.get
        done <- released.get
        closed <- finalized.get
      yield assertTrue(before == 0, result == List(1, 1, 1), pulled == 3, done == 1, closed == 1))
    ,
    test("releasing an unconsumed response closes the transport without pulling its entity"):
      run(for
        read <- IO.ref(false)
        released <- IO.ref(false)
        source = Stream.eval(read.set(true)).drain ++ Stream.never[IO]
        _ <- Http4s
          .client(payload, base, transport(source, released.set(true)))
          .resource(download)(())
          .use(_ => IO.unit)
        pulled <- read.get
        done <- released.get
      yield assertTrue(!pulled, done))
    ,
    test("cancellation during consumption releases the transport and source"):
      run(for
        started <- IO.deferred[Unit]
        released <- IO.ref(false)
        finalized <- IO.ref(false)
        source = (Stream.eval(started.complete(())).drain ++ Stream.never[IO]).onFinalize(finalized.set(true))
        fiber <- Http4s
          .client(payload, base, transport(source, released.set(true)))
          .resource(download)(())
          .use(_.compile.drain)
          .start
        _ <- started.get
        _ <- fiber.cancel
        outcome <- fiber.join
        done <- released.get
        closed <- finalized.get
      yield assertTrue(outcome.isCanceled, done, closed))
    ,
    test("a malformed later element retains endpoint, direction, index, syntax cause and releases resources"):
      run(
        for
          released <- IO.ref(false)
          source = Stream.emits(bytes("1\nnot-json\n").toArray)
          result <- Http4s
            .client(payload, base, transport(source, released.set(true)))
            .resource(download)(())
            .use(_.compile.toList)
            .attempt
          done <- released.get
        yield assertTrue(
          done,
          result.left.exists {
            case failure: Http4sFailure.Streaming =>
              failure.endpoint == download && failure.direction == Http4sFailure.Direction.Response &&
              failure.index.contains(
                1L
              ) && failure.failure.category == Failure.Category.Syntax && Option(failure.getCause).nonEmpty &&
              failure.failure.violations.exists(v => Http4s.report(v).contains("$.body[1]"))
            case _ => false
          }
        )
      )
    ,
    test("request element validation selects the declared error policy"):
      val app = Http4s.app[IO](Route(upload, (values: Stream[IO, Int]) => values.compile.count.map(_.toInt)))(payload)
      val request = HttpRequest[IO](
        method = org.http4s.Method.POST,
        uri = base / "rows",
        headers = HttpHeaders(Header.Raw(CIString("Content-Type"), "application/x-ndjson")),
        entity = Entity.strict(bytes("1\n\"wrong\"\n"))
      )
      run(app(request)).map(result => assertTrue(result.status.code == 422))
    ,
    test("a buffered declared error remains a value beside a streamed success"):
      val app = Http4s.app[IO](
        Route(download, (_: Unit) => IO.raiseError[Stream[IO, Int]](new IllegalStateException("handler")))
      )(payload)
      val call = Http4s
        .client(payload, base, Client.fromHttpApp(app))
        .withApi(Api(errorPolicy.default, unroutedPolicy.default))
        .resource(download)
      run(call(()).use(answer => IO.pure(answer.left.toOption)))
        .map(answer => assertTrue(answer.contains(status.internalServerError)))
    ,
    test("an optional untyped request uses bounded lookahead without closing its source"):
      val optional = endpoint(request(method.post, __)(body.optional(rows)), response(status.ok)(body.json(json.int)))
      val app = Http4s.app[IO](
        Route(optional, (values: Option[Stream[IO, Int]]) => values.fold(IO.pure(-1))(_.compile.count.map(_.toInt)))
      )(payload)
      run(for
        alive <- IO.ref(false)
        source = Stream.bracket(alive.set(true))(_ => alive.set(false)) >> Stream.emits(bytes("1\n").toArray) ++
          Stream
            .eval(alive.get.flatMap(value => IO.raiseUnless(value)(new IllegalStateException("premature release"))))
            .drain
        present <- app(HttpRequest[IO](method = org.http4s.Method.POST, uri = base, entity = Entity.stream(source)))
        content <- Http4sEnvelope.toBytes(present.entity)
        missing <- app(
          HttpRequest[IO](method = org.http4s.Method.POST, uri = base, entity = Entity.stream(Stream.empty))
        )
        absent <- Http4sEnvelope.toBytes(missing.entity)
        released <- alive.get
      yield assertTrue(content.decodeUtf8 == Right("1"), absent.decodeUtf8 == Right("-1"), !released))
    ,
    test("ambiguous streaming alternatives fail during client and route construction"):
      val sameMedia = body
        .streamed[Flow](MediaType("APPLICATION", "X-NDJSON").withParameter("charset", "utf-8"), Frame.Lines, json.int)
      val ambiguous = endpoint(request(method.get, __), response(status.ok)(rows) :+ response(status.ok)(sameMedia))
      assertTrue(
        scala.util.Try(Http4s.client(payload, base, transport(Stream.empty)).resource(ambiguous)).isFailure,
        scala.util
          .Try(Http4s.app[IO](Route(ambiguous, (_: Unit) => IO.pure(Left(Stream.empty[IO]))))(payload))
          .isFailure
      )
    ,
    test("late response failures keep the status, release the source and notify the observer"):
      run(
        for
          events <- IO.ref(List.empty[Http4sObservation.Event])
          finalized <- IO.ref(false)
          cause = new IllegalStateException("late producer failure")
          source = (Stream.emit(1).covary[IO] ++ Stream.raiseError[IO](cause)).onFinalize(finalized.set(true))
          app = Http4s.app[IO](Route(download, (_: Unit) => IO.pure(source)))(
            payload,
            observation => events.update(_ :+ observation.event)
          )
          answer <- app(HttpRequest[IO](uri = base / "rows"))
          result <- answer.body.compile.drain.attempt
          observed <- events.get
          closed <- finalized.get
        yield assertTrue(
          answer.status.code == 200,
          result.isLeft,
          closed,
          observed.exists {
            case Http4sObservation.Event.Failed(failure) =>
              failure.category == Failure.Category.Encoding && failure.cause.contains(cause)
            case _ => false
          }
        )
      )
    ,
    test("typed conversions preserve streamed values in requests and responses"):
      val converted = endpoint(request(method.post, __)(rows).to[Batch], response(status.ok)(rows).to[Batch])
      val app = Http4s.app[IO](Route(converted, (batch: Batch) => IO.pure(batch)))(payload)
      run(
        Http4s
          .client(payload, base, Client.fromHttpApp(app))
          .resource(converted)(Batch(Stream.emits(List(1, 2))))
          .use(_.values.compile.toList)
      ).map(result => assertTrue(result == List(1, 2)))
    ,
    test("media essence selects streamed alternatives and absent content type refuses to guess"):
      val alternatives =
        endpoint(request(method.post, __)(rows :+ body.json(json.int)), response(status.ok)(body.json(json.int)))
      val app = Http4s.app[IO](
        Route(alternatives, (values: Either[Flow[Int], Int]) => values.fold(_.compile.count.map(_.toInt), IO.pure))
      )(payload)
      run(for
        pulled <- IO.ref(false)
        source = Stream.eval(pulled.set(true)).drain ++ Stream.emits(bytes("1\n2\n").toArray)
        missing <- app(HttpRequest[IO](method = org.http4s.Method.POST, uri = base, entity = Entity.stream(source)))
        read <- pulled.get
        streamed <- app(
          HttpRequest[IO](
            method = org.http4s.Method.POST,
            uri = base,
            headers = HttpHeaders(Header.Raw(CIString("Content-Type"), "APPLICATION/X-NDJSON; charset=utf-8")),
            entity = Entity.stream(source)
          )
        )
        count <- Http4sEnvelope.toBytes(streamed.entity)
      yield assertTrue(missing.status.code == 415, !read, streamed.status.code == 200, count.decodeUtf8 == Right("2")))
    ,
    test("request cancellation remains cancellation and releases its source"):
      run(for
        started <- IO.deferred[Unit]
        released <- IO.ref(false)
        events <- IO.ref(List.empty[Http4sObservation.Event])
        app = Http4s.app[IO](Route(upload, (values: Flow[Int]) => values.compile.count.map(_.toInt)))(
          payload,
          observation => events.update(_ :+ observation.event)
        )
        source = (Stream.eval(started.complete(())).drain ++ Stream.never[IO]).onFinalize(released.set(true))
        fiber <- app(
          HttpRequest[IO](
            method = org.http4s.Method.POST,
            uri = base / "rows",
            headers = HttpHeaders(Header.Raw(CIString("Content-Type"), "application/x-ndjson")),
            entity = Entity.stream(source)
          )
        ).start
        _ <- started.get
        _ <- fiber.cancel
        outcome <- fiber.join
        closed <- released.get
        observed <- events.get
      yield assertTrue(outcome.isCanceled, closed, observed == List(Http4sObservation.Event.Cancelled)))
    ,
    test("completing a response releases both source and transport"):
      run(for
        sourceClosed <- IO.ref(false)
        transportClosed <- IO.ref(false)
        source = Stream.emits(bytes("1\n2\n").toArray).covary[IO].onFinalize(sourceClosed.set(true))
        result <- Http4s
          .client(payload, base, transport(source, transportClosed.set(true)))
          .resource(download)(())
          .use(_.compile.toList)
        sourceDone <- sourceClosed.get
        transportDone <- transportClosed.get
      yield assertTrue(result == List(1, 2), sourceDone, transportDone))
    ,
    test("a streamed declared error requires Resource, including API-bound and composed endpoints"):
      assertTrue(
        typeChecks("""
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        import cats.effect.IO
        import cats.syntax.all.*
        import org.http4s.implicits.*
        val client = Http4s.client(Http4sStreamingTest.payload, uri"http://test", org.http4s.client.Client.fromHttpApp(org.http4s.HttpApp[IO](_ => IO.pure(org.http4s.Response[IO]()))))
        val answer = response(status.internalServerError)(Http4sStreamingTest.rows).dimap[Failure, Http4sStreamingTest.Flow[Int]](_ => fs2.Stream.emit(1))(identity)
        val policy = errorPolicy.from(answer)()
        val declared = endpoint(request(method.get, __), response(status.noContent))
        client.resource(policy(declared).client)
        client.withApi(Api(policy, unroutedPolicy.default)).resource(declared)
      """),
        !typeChecks("""
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.dsl.*
        import cats.effect.IO
        import cats.syntax.all.*
        import org.http4s.implicits.*
        val client = Http4s.client(Http4sStreamingTest.payload, uri"http://test", org.http4s.client.Client.fromHttpApp(org.http4s.HttpApp[IO](_ => IO.pure(org.http4s.Response[IO]()))))
        val answer = response(status.internalServerError)(Http4sStreamingTest.rows).dimap[Failure, Http4sStreamingTest.Flow[Int]](_ => fs2.Stream.emit(1))(identity)
        val policy = errorPolicy.from(answer)()
        val declared = endpoint(request(method.get, __), response(status.noContent))
        client.withApi(Api(policy, unroutedPolicy.default))(declared)
      """)
      )
    ,
    test("registration, carriers and response lifetime are checked by the compiler"):
      assertTrue(
        typeChecks("""
          import io.taig.otter.http.*
          import cats.effect.IO
          import org.http4s.implicits.*
          val client = Http4s.client(Http4sStreamingTest.payload, uri"http://test", org.http4s.client.Client.fromHttpApp(org.http4s.HttpApp[IO](_ => IO.pure(org.http4s.Response[IO]()))))
          client.resource(Http4sStreamingTest.download)
          client(Http4sStreamingTest.upload)
        """),
        !typeChecks("""
          import io.taig.otter.http.*
          import cats.effect.IO
          import org.http4s.implicits.*
          val client = Http4s.client(Http4sStreamingTest.payload, uri"http://test", org.http4s.client.Client.fromHttpApp(org.http4s.HttpApp[IO](_ => IO.pure(org.http4s.Response[IO]()))))
          client(Http4sStreamingTest.download)
        """),
        !typeChecks("""
          import io.taig.otter.http.*
          import cats.effect.IO
          Http4s.app[IO](Route(Http4sStreamingTest.download, (_: Unit) => IO.pure(fs2.Stream.empty[IO])))(Http4sCirce.Payload)
        """),
        !typeChecks("""
          import io.taig.otter.http.*
          import io.taig.otter.http.fixture.dsl.*
          import io.taig.otter.http.fixture.payload as json
          import cats.effect.IO
          val endpoint = io.taig.otter.http.fixture.dsl.endpoint(request(method.get, __), response(status.ok)(body.ndjson[List](json.int)))
          Http4s.app[IO](Route(endpoint, (_: Unit) => IO.pure(List(1))))(Http4sStreamingTest.payload)
        """),
        !typeChecks("""
          import io.taig.otter.http.*
          import cats.effect.IO
          Http4s.app[IO](Route(Http4sStreamingTest.download, (_: Unit) => IO.pure(fs2.Stream.emit("wrong"))))(Http4sStreamingTest.payload)
        """)
      )
  ) @@ TestAspect.timeout(zio.Duration.fromSeconds(20))
