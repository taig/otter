package io.taig.otter.http

import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.codec.Http4sResultDecoder
import io.taig.otter.http.fixture.*
import io.taig.otter.http.fixture.dsl.*
import org.http4s.Request as Http4sRequest
import org.http4s.Uri
import org.http4s.client.Client as Http4sClient
import org.http4s.implicits.*
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

/** One endpoint value, served and called, with nothing in between.
  *
  * The strongest claim this module can make, and the reason it is worth more than a pair of one sided tests: a
  * `Client.fromHttpApp` hands the routes the very request the client interpreter built, so a disagreement between the
  * two sides is a failure here rather than something that shows up against a real server. Both halves are driven by the
  * *same* endpoint value -- read once as `Endpoint.Server` and once as `Endpoint.Client` -- so nothing can agree by
  * being written twice the same wrong way.
  *
  * No socket is opened, which is what lets this suite run on Scala.js as well as the JVM.
  */
object Http4sRoundTripTest extends ZIOSpecDefault:
  private val Base: Uri = uri"http://otter.test"

  /** `GET /reports/{id}?page`, answering with a report or saying there is none. */
  private val fetch: Endpoint[(Int, Int), Either[Report, Unit]] =
    endpoint(
      request(Method.Get, api.one).queries(api.paging),
      result(Code.Ok).body(json(api.report)) :+ result(Code.NotFound)
    )

  /** `PUT /settings`, whose payload has a defaulted field and whose answer has no entity at all. */
  private val configure: Endpoint[Settings, Unit] =
    endpoint(request(Method.Put, PNil :* segment("settings")).body(json(api.settings)), result(Code.NoContent).toUnion)

  /** `GET /trees`, whose payload refers to itself. */
  private val trees: Endpoint[Unit, Tree] =
    endpoint(request(Method.Get, PNil :* segment("trees")), result(Code.Ok).body(json(api.tree)).toUnion)

  /** `POST /files` taking and answering with bytes that have no document in them at all. */
  private val upload: Endpoint[ByteVector, ByteVector] =
    endpoint(
      request(Method.Post, PNil :* segment("files")).body(body.binary(MediaType.Pdf)),
      result(Code.Ok).body(body.binary(MediaType.Pdf)).toUnion
    )

  /** `POST /reports` whose body may be either of two alternatives, which is what content negotiation describes. */
  private val negotiated: Endpoint[Either[Report, ByteVector], Unit] =
    endpoint(
      request(Method.Post, PNil :* segment("reports")).bodies(api.negotiated),
      result(Code.NoContent).toUnion
    )

  /** `POST /uploads`, whose payload is a set of parts -- a payload alphabet no interpreter here recognises. */
  private val multipart: Endpoint[Upload, Unit] =
    endpoint(
      request(Method.Post, PNil :* segment("uploads")).body(body.multipart(api.upload)),
      result(Code.NoContent).toUnion
    )

  /** `GET /reports`, whose answer this interpreter cannot yet carry. */
  private val streaming: Endpoint[Unit, Unit] =
    endpoint(request(Method.Get, PNil :* segment("reports")), result(Code.Ok).streaming(api.reports).toUnion)

  private def routes[A, B](endpoint: Endpoint[A, B], handler: A => IO[B]): Http4sClient[IO] =
    Http4sClient.fromHttpApp(Http4s.routes[IO](Http4sCirce.Payload)(Route(endpoint, handler)).orNotFound)

  /** The value a caller gets back for the value it sent, having gone the whole way round. */
  private def roundTrip[A, B](endpoint: Endpoint[A, B], handler: A => IO[B])(value: A): Task[B] =
    ZIO.fromFuture: _ =>
      Http4s.client[IO, A, B](Http4sCirce.Payload, Base, routes(endpoint, handler))(endpoint)(value).unsafeToFuture()

  /** The request as the handler saw it, which is the half a returned value cannot show. */
  private def received[A, B](endpoint: Endpoint[A, B], answer: B)(value: A): Task[A] =
    ZIO.fromFuture: _ =>
      IO.ref(Option.empty[A])
        .flatMap: ref =>
          val handler = (received: A) => ref.set(Some(received)).as(answer)

          Http4s.client[IO, A, B](Http4sCirce.Payload, Base, routes(endpoint, handler))(endpoint)(value) *> ref.get
        .map(_.get)
        .unsafeToFuture()

  private def send[A](endpoint: Endpoint[A, Unit], request: Http4sRequest[IO]): Task[Int] =
    ZIO.fromFuture: _ =>
      Http4s
        .routes[IO](Http4sCirce.Payload)(Route(endpoint, (_: A) => IO.unit))
        .orNotFound
        .run(request)
        .map(_.status.code)
        .unsafeToFuture()

  /** `GET /reports/{id}?page` answering with nothing, which is enough to ask a router questions with. */
  private val ping: Endpoint[(Int, Int), Unit] =
    endpoint(request(Method.Get, api.one).queries(api.paging), result(Code.NoContent).toUnion)

  /** `GET /ping` reading a header, which is the one envelope position the other fixtures do not use. */
  private val headed: Endpoint[(String, Option[List[String]]), Unit] =
    endpoint(
      request(Method.Get, PNil :* segment("ping")).headers(http.request),
      result(Code.NoContent).toUnion
    )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sRoundTripTest")(
    suite("both sides of one endpoint")(
      test("a report written by the handler is the report the caller reads"):
        val report = Report("Quarterly", 12)

        roundTrip(fetch, (_: (Int, Int)) => IO.pure(Left(report)))((42, 3)).map(answer =>
          assertTrue(answer == Left(report))
        )
      ,
      test("the branch the handler took is the branch the caller reads, chosen by the status code"):
        roundTrip(fetch, (_: (Int, Int)) => IO.pure(Right(())))((42, 3)).map(answer => assertTrue(answer == Right(())))
      ,
      test("the handler is given exactly the path and query the caller wrote"):
        received(fetch, Right(()))((42, 3)).map(seen => assertTrue(seen == (42, 3)))
      ,
      test("a query left to its default reaches the handler as the default"):
        received(fetch, Right(()))((42, 1)).map(seen => assertTrue(seen == (42, 1)))
      ,
      test("a body the caller wrote is the body the handler reads"):
        received(configure, ())(Settings("light")).map(seen => assertTrue(seen == Settings("light")))
      ,
      test("an answer with no entity still round trips"):
        roundTrip(configure, (_: Settings) => IO.unit)(Settings("light")).map(answer => assertTrue(answer == ()))
      ,
      test("a body of bytes with no document in it round trips unchanged"):
        val bytes = ByteVector(0x25, 0x50, 0x44, 0x46, 0x00, 0xff)

        roundTrip(upload, (received: ByteVector) => IO.pure(received))(bytes).map(answer => assertTrue(answer == bytes))
      ,
      test("a header the caller wrote is a header the handler reads"):
        received(headed, ())(("abc-123", None)).map(seen => assertTrue(seen == ("abc-123", None)))
      ,
      test("a payload that refers to itself round trips to any depth"):
        val tree = Tree(1, List(Tree(2, List(Tree(3, Nil))), Tree(4, Nil)))

        roundTrip(trees, (_: Unit) => IO.pure(tree))(()).map(answer => assertTrue(answer == tree))
    ),
    suite("routing")(
      test("a path this endpoint does not describe falls through, and is not a bad request"):
        send(ping, Http4sRequest[IO](uri = uri"http://otter.test/orders/42")).map(code => assertTrue(code == 404))
      ,
      test("a path of the right shape but the wrong literal falls through too"):
        send(ping, Http4sRequest[IO](uri = uri"http://otter.test/orders/42?page=1")).map(code =>
          assertTrue(code == 404)
        )
      ,
      test("a segment too many is a different path and not a malformed one"):
        send(ping, Http4sRequest[IO](uri = uri"http://otter.test/reports/42/extra")).map(code =>
          assertTrue(code == 404)
        )
      ,
      test("the method is part of what a route matches on"):
        send(ping, Http4sRequest[IO](method = org.http4s.Method.DELETE, uri = uri"http://otter.test/reports/42"))
          .map(code => assertTrue(code == 404))
      ,
      test("a path that matches but does not hold what it describes is a bad request"):
        send(ping, Http4sRequest[IO](uri = uri"http://otter.test/reports/not-a-number"))
          .map(code => assertTrue(code == 400))
      ,
      test("a query that does not hold what it describes is a bad request"):
        send(ping, Http4sRequest[IO](uri = uri"http://otter.test/reports/42?page=soon"))
          .map(code => assertTrue(code == 400))
    ),
    suite("content negotiation")(
      test("the alternative the caller wrote is the one the handler reads"):
        received(negotiated, ())(Left(Report("Quarterly", 12))).map(seen =>
          assertTrue(seen == Left(Report("Quarterly", 12)))
        )
      ,
      test("the other alternative is told apart by its media type and not by parsing"):
        val bytes = ByteVector(0x25, 0x50, 0x44, 0x46)

        received(negotiated, ())(Right(bytes)).map(seen => assertTrue(seen == Right(bytes)))
    ),
    suite("shortfalls")(
      test("a streamed answer is reported rather than answered with an empty body"):
        roundTrip(streaming, (_: Unit) => IO.unit)(()).exit
          .map(exit => assertTrue(exit.isFailure))
      ,
      test("a payload alphabet nothing recognises is reported, and names the media type"):
        roundTrip(multipart, (_: Upload) => IO.unit)(Upload(Report("Quarterly", 12), ByteVector.empty)).exit
          .map(exit =>
            assertTrue(
              exit.causeOption.exists(_.failures.exists {
                case Http4sFailure.Interpreter(Http4sIssue.Uninterpreted(mediaType)) =>
                  mediaType == MediaType.MultipartFormData
                case _ => false
              })
            )
          )
      ,
      test("a response under a status no branch names says which it expected"):
        val report = Http4sResultDecoder(Http4sCirce.Payload)
          .decode(fetch.responses, Http4sWire.Response(Code(500), Chain.empty, None))
          .swap
          .toOption
          .map(Http4s.report)

        assertTrue(report.exists(_.contains("oneof"))) && assertTrue(report.exists(_.contains("500")))
    ),
    suite("reporting")(
      test("a malformed parameter is reported at the position it was found"):
        val report = Http4sRequestDecoder(Http4sCirce.Payload)
          .decode(
            ping.request,
            Http4sWire.Request(Vector("reports", "nope"), Chain.empty, Chain.empty, (None, ByteVector.empty))
          )
          .swap
          .toOption
          .map(Http4s.report)

        assertTrue(report.exists(_.contains(".path"))) && assertTrue(report.exists(_.contains(".id")))
    )
  )
