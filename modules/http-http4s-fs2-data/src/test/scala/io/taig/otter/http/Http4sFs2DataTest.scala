package io.taig.otter.http

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.taig.otter.Csv
import io.taig.otter.Reference
import io.taig.otter.component.CsvComponent
import io.taig.otter.fixture.Book
import io.taig.otter.fixture.csv
import io.taig.otter.http.codec.Http4sBodyEncoder
import io.taig.otter.http.component.BodyComponent
import io.taig.otter.http.component.HttpComponent
import io.taig.otter.http.fixture.Report
import io.taig.otter.http.fixture.api
import io.taig.otter.http.fixture.dsl
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.syntax.HttpCsvSyntax
import io.taig.otter.http.syntax.HttpJsonSyntax
import org.http4s.client.Client as Http4sClient
import org.http4s.implicits.*
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

import scala.compiletime.testing.typeChecks

/** CSV payloads participate in the same typed HTTP interpreter boundary as JSON payloads. */
object Http4sFs2DataTest extends ZIOSpecDefault:
  private object mixedDsl extends HttpComponent:
    override val body: BodyComponent & HttpJsonSyntax & HttpCsvSyntax =
      new BodyComponent with HttpJsonSyntax with HttpCsvSyntax {}

  private val mixed =
    mixedDsl.endpoint(
      mixedDsl.request(method.post, __)(api.reported :+ mixedDsl.body.csv(csv.book)),
      mixedDsl.response(status.noContent)
    )

  val multipart = mixedDsl.endpoint(
    request(method.post, __)(body.multipart(part("json", api.reported) :* part("csv", mixedDsl.body.csv(csv.book)))),
    response(status.noContent)
  )

  private val jsonEndpoint =
    mixedDsl.endpoint(
      mixedDsl.request(method.post, __ :* segment("json"))(api.reported),
      mixedDsl.response(status.noContent)
    )

  private val csvEndpoint =
    mixedDsl.endpoint(
      mixedDsl.request(method.post, __ :* segment("csv"))(mixedDsl.body.csv(csv.book)),
      mixedDsl.response(status.noContent)
    )

  private val jsonResponse =
    mixedDsl.endpoint(
      mixedDsl.request(method.get, __ :* segment("json-response")),
      mixedDsl.response(status.ok)(mixedDsl.body.json(api.report)).toUnion
    )

  private val positional: Csv.Tuple[Book] =
    (CsvComponent.TNil :* CsvComponent.string :* CsvComponent.int :* CsvComponent.boolean).to

  private val emptyRecord: Csv.Record[Unit] = CsvComponent.RNil

  private val emptyTuple: Csv.Tuple[Unit] = CsvComponent.TNil

  val routeGroups =
    Routes(
      Route(jsonEndpoint, (_: Report) => IO.unit)
    ) ++ Routes(
      Route(csvEndpoint, (_: Book) => IO.unit)
    )

  val payloads = Http4sCirce.Payload.orElse(Http4sFs2Data.Payload)

  private def call(value: Either[Report, Book]): Task[Either[Report, Book]] =
    ZIO.fromFuture: _ =>
      IO.ref(Option.empty[Either[Report, Book]])
        .flatMap: ref =>
          val routes =
            Http4s
              .routes[IO](
                Route(mixed, (received: Either[Report, Book]) => ref.set(Some(received)))
              )(payloads)
              .orNotFound
          val client = Http4sClient.fromHttpApp(routes)
          Http4s.client(payloads, uri"http://otter.test", client)(mixed)(value) *>
            ref.get.map(_.get)
        .unsafeToFuture()

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sFs2DataTest")(
    test("JSON and CSV multipart parts require registration inside the multipart interpreter"):
      val prelude = """
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.Report
        import io.taig.otter.fixture.Book
        import org.http4s.implicits.*
        val route = Route(Http4sFs2DataTest.multipart, (_: (Report, Book)) => IO.unit)
      """
      assertTrue(
        typeChecks(prelude + "Http4s.app[IO](route)(Http4sMultipart.payload(Http4sFs2DataTest.payloads))"),
        !typeChecks(prelude + "Http4s.app[IO](route)(Http4sMultipart.payload(Http4sCirce.Payload))"),
        !typeChecks(
          prelude + "Http4s.app[IO](route)(Http4sMultipart.payload(Http4sCirce.Payload).orElse(Http4sFs2Data.Payload))"
        )
      )
    ,
    test("registered JSON and CSV multipart parts reach the handler"):
      val interpreter = Http4sMultipart.payload(payloads)
      val value = (Report("Quarterly", 12), Book("Dune", 412, true))
      ZIO.fromFuture(_ =>
        (for
          seen <- IO.ref(Option.empty[(Report, Book)])
          transport = Http4sClient.fromHttpApp(
            Http4s.app[IO](Route(multipart, (received: (Report, Book)) => seen.set(Some(received))))(interpreter)
          )
          _ <- Http4s.client(interpreter, uri"http://otter.test", transport)(multipart)(value)
          received <- seen.get
        yield assertTrue(received.contains(value))).unsafeToFuture()
      )
    ,
    test("one configured client calls endpoints in both payload alphabets"):
      val transport = Http4sClient.fromHttpApp(Http4s.routes[IO](routeGroups)(payloads).orNotFound)
      val client = Http4s.client(payloads, uri"http://otter.test", transport)
      val json = client(jsonEndpoint)
      val csv = client(csvEndpoint)
      ZIO.fromFuture: _ =>
        (for
          a <- json(Report("Quarterly", 12))
          b <- csv(Book("Dune", 412, true))
        yield assertTrue(a == (), b == ())).unsafeToFuture()
    ,
    test("configured client requirements include every alphabet in a mixed endpoint"):
      val prelude = """
        import cats.effect.IO
        import io.taig.otter.http.*
        import org.http4s.implicits.*
        val transport = org.http4s.client.Client.fromHttpApp(org.http4s.HttpApp[IO](_ => IO.pure(org.http4s.Response[IO]())))
      """
      assertTrue(
        typeChecks(prelude + """
          val client = Http4s.client(Http4sFs2DataTest.payloads, uri"http://test", transport)
          client(Http4sFs2DataTest.mixed)
        """),
        !typeChecks(prelude + """
          val client = Http4s.client(Http4sCirce.Payload, uri"http://test", transport)
          client(Http4sFs2DataTest.mixed)
        """),
        !typeChecks(prelude + """
          val client = Http4s.client(Http4sFs2Data.Payload, uri"http://test", transport)
          client(Http4sFs2DataTest.jsonResponse)
        """)
      )
    ,
    test("request encoding failures fail the effect without acquiring the transport"):
      val invalid = mixedDsl.endpoint(
        mixedDsl.request(method.post, __)(mixedDsl.body.csv(emptyRecord)),
        mixedDsl.response(status.noContent)
      )
      ZIO.fromFuture: _ =>
        (for
          called <- IO.ref(false)
          transport = Http4sClient[IO](_ => cats.effect.Resource.eval(called.set(true).as(org.http4s.Response[IO]())))
          client = Http4s.client(Http4sFs2Data.Payload, uri"http://otter.test", transport)
          result <- client(invalid)(()).attempt
          acquired <- called.get
        yield assertTrue(
          result == Left(
            Http4sFailure.Encoding(
              Http4sIssue.Encoding(MediaType("text", "csv"), "A CSV record must have at least one column")
            )
          ),
          !acquired
        )).unsafeToFuture()
    ,
    test("single-column records and tuples preserve empty cells and carriage returns"):
      val tuple = CsvDocument.Tuple(Reference.now(CsvComponent.TNil :* CsvComponent.string))
      val record = CsvDocument.Record(
        Reference.now(CsvComponent.RNil :* CsvComponent.field("value", CsvComponent.string))
      )
      val values = Vector("", "x", "", "x\r", "\r", "x\r\ny", "a,b", "a\"b", "a\nb", "")
      val documents: Vector[CsvDocument.Row[String, String]] = Vector(tuple, record)

      assertTrue(documents.forall: document =>
        val singles = values.forall: value =>
          val encoded = Http4sFs2Data.Codec.encode(document, value).toOption
          encoded.flatMap(Http4sFs2Data.Codec.decodeDetailed[String](document, _).toOption).contains(value)
        val collection = CsvDocument.Rows(Reference.now(document))
        val encoded = Http4sFs2Data.Codec.encode(collection, values).toOption
        val decoded = encoded
          .flatMap(Http4sFs2Data.Codec.decodeDetailed[Vector[String]](collection, _).toOption)
        singles && decoded.contains(values))
    ,
    test("an empty record header is preserved"):
      val document = CsvDocument.Record(
        Reference.now(CsvComponent.RNil :* CsvComponent.field("", CsvComponent.string))
      )
      val encoded = Http4sFs2Data.Codec.encode(document, "value").toOption
      val decoded = encoded.flatMap(Http4sFs2Data.Codec.decodeDetailed[String](document, _).toOption)

      assertTrue(decoded.contains("value"))
    ,
    test("a mixed JSON and CSV route requires both interpreters"):
      assertTrue(typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        import io.taig.otter.http.fixture.Report
        import io.taig.otter.http.fixture.dsl.*
        import io.taig.otter.fixture.csv
        val endpoint = Http4sFs2DataTest.mixed
        Http4s.routes[IO](Route(endpoint,
          (_: Either[Report, io.taig.otter.fixture.Book]) => IO.unit))(Http4sCirce.Payload.orElse(Http4sFs2Data.Payload))
      """))
    ,
    test("a JSON request selects the JSON interpreter"):
      call(Left(Report("Quarterly", 12))).map(value => assertTrue(value == Left(Report("Quarterly", 12))))
    ,
    test("a CSV request selects the CSV interpreter"):
      call(Right(Book("Dune", 412, true))).map(value => assertTrue(value == Right(Book("Dune", 412, true))))
    ,
    test("omitting the CSV interpreter is rejected at route construction"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        val endpoint = Http4sFs2DataTest.mixed
        Http4s.routes[IO](Route(endpoint,
          (_: Either[io.taig.otter.http.fixture.Report, io.taig.otter.fixture.Book]) => IO.unit))(Http4sCirce.Payload)
      """))
    ,
    test("response requirements are checked too"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        val endpoint = Http4sFs2DataTest.jsonResponse
        Http4s.routes[IO](Route(endpoint,
          (_: Unit) => IO.pure(io.taig.otter.http.fixture.Report("Quarterly", 12))))(Http4sFs2Data.Payload)
      """))
    ,
    test("independently composed route groups retain their combined requirements"):
      assertTrue(typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        Http4s.routes[IO](Http4sFs2DataTest.routeGroups)(Http4sFs2DataTest.payloads)
      """))
    ,
    test("a route group cannot be mounted with only one interpreter"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        Http4s.routes[IO](Http4sFs2DataTest.routeGroups)(Http4sCirce.Payload)
      """))
    ,
    test("the CSV interpreter preserves quoted cells"):
      val text = "title,pages,read\n\"Dune, Frank\",412,true\n"
      val schema = csv.book
      val decoded = Http4sFs2Data.Codec.decodeDetailed[Book](
        CsvDocument.Record(io.taig.otter.Reference.now(schema)),
        ByteVector.encodeUtf8(text).toOption.get
      )
      assertTrue(decoded.toOption.contains(Book("Dune, Frank", 412, true)))
    ,
    test("a record body writes and reads its header"):
      val document = CsvDocument.Record(Reference.now(csv.book))
      val encoded = Http4sFs2Data.Codec.encode(document, Book("Dune", 412, true)).toOption
      val decoded = encoded.flatMap(bytes => Http4sFs2Data.Codec.decodeDetailed[Book](document, bytes).toOption)

      assertTrue(
        encoded.flatMap(_.decodeUtf8.toOption) == Some("title,pages,read\nDune,412,true\n"),
        decoded == Some(Book("Dune", 412, true))
      )
    ,
    test("a collection writes multiple positional rows"):
      val document = CsvDocument.Rows(Reference.now(CsvDocument.Tuple(Reference.now(positional))))
      val values = Vector(Book("Dune", 412, true), Book("Emma", 160, false))
      val encoded = Http4sFs2Data.Codec.encode(document, values).toOption
      val decoded =
        encoded.flatMap(bytes => Http4sFs2Data.Codec.decodeDetailed[Vector[Book]](document, bytes).toOption)

      assertTrue(
        encoded.flatMap(_.decodeUtf8.toOption) == Some("Dune,412,true\nEmma,160,false\n"),
        decoded == Some(values)
      )
    ,
    test("an empty record collection still emits its header"):
      val document = CsvDocument.Rows(Reference.now(CsvDocument.Record(Reference.now(csv.book))))
      val encoded = Http4sFs2Data.Codec.encode(document, Vector.empty[Book]).toOption
      val decoded =
        encoded.flatMap(bytes => Http4sFs2Data.Codec.decodeDetailed[Vector[Book]](document, bytes).toOption)

      assertTrue(encoded.flatMap(_.decodeUtf8.toOption) == Some("title,pages,read\n"), decoded == Some(Vector.empty))
    ,
    test("a single positional body has exactly one row"):
      val document = CsvDocument.Tuple(Reference.now(positional))
      val encoded = Http4sFs2Data.Codec.encode(document, Book("Dune", 412, true)).toOption
      val decoded = encoded.flatMap(bytes => Http4sFs2Data.Codec.decodeDetailed[Book](document, bytes).toOption)
      val tooFew = Http4sFs2Data.Codec.decodeDetailed[Book](document, ByteVector.encodeUtf8("Dune,412\n").toOption.get)
      val tooMany = Http4sFs2Data.Codec
        .decodeDetailed[Book](document, ByteVector.encodeUtf8("Dune,412,true\nEmma,160,false\n").toOption.get)

      assertTrue(decoded == Some(Book("Dune", 412, true)), tooFew.isInvalid, tooMany.isInvalid)
    ,
    test("quoted cells may contain embedded newlines"):
      val document = CsvDocument.Record(Reference.now(csv.book))
      val text = "title,pages,read\n\"Dune\nFrank\",412,true\n"
      val decoded = Http4sFs2Data.Codec.decodeDetailed[Book](document, ByteVector.encodeUtf8(text).toOption.get)

      assertTrue(decoded.toOption.contains(Book("Dune\nFrank", 412, true)))
    ,
    test("malformed input and invalid UTF-8 are violations"):
      val document = CsvDocument.Record(Reference.now(csv.book))
      val malformed = ByteVector.encodeUtf8("title,pages,read\n\"Dune,412,true\n").toOption.get
      val invalidUtf8 = ByteVector(0xff.toByte)

      assertTrue(
        Http4sFs2Data.Codec
          .decodeDetailed[Book](document, malformed)
          .swap
          .toOption
          .exists(failure => failure.category == Failure.Category.Syntax && failure.cause.nonEmpty),
        Http4sFs2Data.Codec
          .decodeDetailed[Book](document, invalidUtf8)
          .swap
          .toOption
          .exists(failure => failure.category == Failure.Category.Syntax && failure.cause.nonEmpty)
      )
    ,
    test("collection row failures include their row index"):
      val document = CsvDocument.Rows(Reference.now(CsvDocument.Record(Reference.now(csv.book))))
      val text = "title,pages,read\nDune,412,true\nEmma,nope,false\n"
      val result = Http4sFs2Data.Codec.decodeDetailed[Vector[Book]](document, ByteVector.encodeUtf8(text).toOption.get)

      assertTrue(
        result.fold(
          error => error.category == Failure.Category.Validation && Http4s.report(error.violations).contains("[1]"),
          _ => false
        )
      )
    ,
    test("zero-column output is an encoding failure"):
      val record = CsvDocument.Record(Reference.now(emptyRecord))
      val tupleRows = CsvDocument.Rows(Reference.now(CsvDocument.Tuple(Reference.now(emptyTuple))))
      val body = mixedDsl.body.csv(emptyRecord)
      val requestIssue = Http4sBodyEncoder[IO, CsvDocument](Http4sFs2Data.Payload).encode(body, ())

      ZIO
        .fromFuture(_ => requestIssue.unsafeToFuture())
        .map(issue =>
          assertTrue(
            Http4sFs2Data.Codec.encode(record, ()) == Left("A CSV record must have at least one column"),
            Http4sFs2Data.Codec.encode(tupleRows, Vector.empty[Unit]) ==
              Left("A CSV tuple must have at least one column"),
            issue == Left(
              Http4sIssue.Encoding(MediaType("text", "csv"), "A CSV record must have at least one column")
            )
          )
        )
  )
