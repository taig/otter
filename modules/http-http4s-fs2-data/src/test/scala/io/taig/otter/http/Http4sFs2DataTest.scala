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
      mixedDsl.result(code.noContent)
    )

  private val jsonEndpoint =
    mixedDsl.endpoint(
      mixedDsl.request(method.post, __ :* segment("json"))(api.reported),
      mixedDsl.result(code.noContent)
    )

  private val csvEndpoint =
    mixedDsl.endpoint(
      mixedDsl.request(method.post, __ :* segment("csv"))(mixedDsl.body.csv(csv.book)),
      mixedDsl.result(code.noContent)
    )

  private val jsonResponse =
    mixedDsl.endpoint(
      mixedDsl.request(method.get, __ :* segment("json-response")),
      mixedDsl.result(code.ok)(mixedDsl.body.json(api.report)).toUnion
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
              .routes[IO](payloads)(
                Route(mixed, (received: Either[Report, Book]) => ref.set(Some(received)))
              )
              .orNotFound
          val client = Http4sClient.fromHttpApp(routes)
          Http4s.client[IO, Either[Report, Book], Unit](payloads, uri"http://otter.test", client)(mixed)(value) *>
            ref.get.map(_.get)
        .unsafeToFuture()

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sFs2DataTest")(
    test("single-column records and tuples preserve empty cells and carriage returns"):
      val tuple = CsvDocument.Tuple(Reference.now(CsvComponent.TNil :* CsvComponent.string))
      val record = CsvDocument.Record(
        Reference.now(CsvComponent.RNil :* CsvComponent.field("value", CsvComponent.string))
      )
      val values = Vector("", "x", "", "x\r", "\r", "x\r\ny", "a,b", "a\"b", "a\nb", "")
      val documents: Vector[CsvDocument.Row[String, String]] = Vector(tuple, record)

      assertTrue(documents.forall: document =>
        val singles = values.forall: value =>
          val encoded = Http4sFs2Data.Payload.encode(document, value).flatMap(_.toOption)
          encoded.flatMap(Http4sFs2Data.Payload.decode[String](document, _)).flatMap(_.toOption).contains(value)
        val collection = CsvDocument.Rows(Reference.now(document))
        val encoded = Http4sFs2Data.Payload.encode(collection, values).flatMap(_.toOption)
        val decoded = encoded
          .flatMap(Http4sFs2Data.Payload.decode[Vector[String]](collection, _))
          .flatMap(_.toOption)
        singles && decoded.contains(values))
    ,
    test("an empty record header is preserved"):
      val document = CsvDocument.Record(
        Reference.now(CsvComponent.RNil :* CsvComponent.field("", CsvComponent.string))
      )
      val encoded = Http4sFs2Data.Payload.encode(document, "value").flatMap(_.toOption)
      val decoded = encoded.flatMap(Http4sFs2Data.Payload.decode[String](document, _)).flatMap(_.toOption)

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
        Http4s.routes[IO](Http4sCirce.Payload.orElse(Http4sFs2Data.Payload))(Route(endpoint,
          (_: Either[Report, io.taig.otter.fixture.Book]) => IO.unit))
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
        Http4s.routes[IO](Http4sCirce.Payload)(Route(endpoint,
          (_: Either[io.taig.otter.http.fixture.Report, io.taig.otter.fixture.Book]) => IO.unit))
      """))
    ,
    test("response requirements are checked too"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        val endpoint = Http4sFs2DataTest.jsonResponse
        Http4s.routes[IO](Http4sFs2Data.Payload)(Route(endpoint,
          (_: Unit) => IO.pure(io.taig.otter.http.fixture.Report("Quarterly", 12))))
      """))
    ,
    test("independently composed route groups retain their combined requirements"):
      assertTrue(typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        Http4s.routes[IO](Http4sFs2DataTest.payloads)(Http4sFs2DataTest.routeGroups)
      """))
    ,
    test("a route group cannot be mounted with only one interpreter"):
      assertTrue(!typeChecks("""
        import cats.effect.IO
        import io.taig.otter.http.*
        Http4s.routes[IO](Http4sCirce.Payload)(Http4sFs2DataTest.routeGroups)
      """))
    ,
    test("the CSV interpreter preserves quoted cells"):
      val text = "title,pages,read\n\"Dune, Frank\",412,true\n"
      val schema = csv.book
      val decoded = Http4sFs2Data.Payload.decode[Book](
        CsvDocument.Record(io.taig.otter.Reference.now(schema)),
        ByteVector.encodeUtf8(text).toOption.get
      )
      assertTrue(decoded.flatMap(_.toOption).contains(Book("Dune, Frank", 412, true)))
    ,
    test("a record body writes and reads its header"):
      val document = CsvDocument.Record(Reference.now(csv.book))
      val encoded = Http4sFs2Data.Payload.encode(document, Book("Dune", 412, true)).flatMap(_.toOption)
      val decoded = encoded.flatMap(bytes => Http4sFs2Data.Payload.decode[Book](document, bytes).flatMap(_.toOption))

      assertTrue(
        encoded.flatMap(_.decodeUtf8.toOption) == Some("title,pages,read\nDune,412,true\n"),
        decoded == Some(Book("Dune", 412, true))
      )
    ,
    test("a collection writes multiple positional rows"):
      val document = CsvDocument.Rows(Reference.now(CsvDocument.Tuple(Reference.now(positional))))
      val values = Vector(Book("Dune", 412, true), Book("Emma", 160, false))
      val encoded = Http4sFs2Data.Payload.encode(document, values).flatMap(_.toOption)
      val decoded =
        encoded.flatMap(bytes => Http4sFs2Data.Payload.decode[Vector[Book]](document, bytes).flatMap(_.toOption))

      assertTrue(
        encoded.flatMap(_.decodeUtf8.toOption) == Some("Dune,412,true\nEmma,160,false\n"),
        decoded == Some(values)
      )
    ,
    test("an empty record collection still emits its header"):
      val document = CsvDocument.Rows(Reference.now(CsvDocument.Record(Reference.now(csv.book))))
      val encoded = Http4sFs2Data.Payload.encode(document, Vector.empty[Book]).flatMap(_.toOption)
      val decoded =
        encoded.flatMap(bytes => Http4sFs2Data.Payload.decode[Vector[Book]](document, bytes).flatMap(_.toOption))

      assertTrue(encoded.flatMap(_.decodeUtf8.toOption) == Some("title,pages,read\n"), decoded == Some(Vector.empty))
    ,
    test("a single positional body has exactly one row"):
      val document = CsvDocument.Tuple(Reference.now(positional))
      val encoded = Http4sFs2Data.Payload.encode(document, Book("Dune", 412, true)).flatMap(_.toOption)
      val decoded = encoded.flatMap(bytes => Http4sFs2Data.Payload.decode[Book](document, bytes).flatMap(_.toOption))
      val tooFew = Http4sFs2Data.Payload.decode[Book](document, ByteVector.encodeUtf8("Dune,412\n").toOption.get)
      val tooMany = Http4sFs2Data.Payload
        .decode[Book](document, ByteVector.encodeUtf8("Dune,412,true\nEmma,160,false\n").toOption.get)

      assertTrue(decoded == Some(Book("Dune", 412, true)), tooFew.exists(_.isInvalid), tooMany.exists(_.isInvalid))
    ,
    test("quoted cells may contain embedded newlines"):
      val document = CsvDocument.Record(Reference.now(csv.book))
      val text = "title,pages,read\n\"Dune\nFrank\",412,true\n"
      val decoded = Http4sFs2Data.Payload.decode[Book](document, ByteVector.encodeUtf8(text).toOption.get)

      assertTrue(decoded.flatMap(_.toOption).contains(Book("Dune\nFrank", 412, true)))
    ,
    test("malformed input and invalid UTF-8 are violations"):
      val document = CsvDocument.Record(Reference.now(csv.book))
      val malformed = ByteVector.encodeUtf8("title,pages,read\n\"Dune,412,true\n").toOption.get
      val invalidUtf8 = ByteVector(0xff.toByte)

      assertTrue(
        Http4sFs2Data.Payload.decode[Book](document, malformed).exists(_.isInvalid),
        Http4sFs2Data.Payload.decode[Book](document, invalidUtf8).exists(_.isInvalid)
      )
    ,
    test("collection row failures include their row index"):
      val document = CsvDocument.Rows(Reference.now(CsvDocument.Record(Reference.now(csv.book))))
      val text = "title,pages,read\nDune,412,true\nEmma,nope,false\n"
      val result = Http4sFs2Data.Payload.decode[Vector[Book]](document, ByteVector.encodeUtf8(text).toOption.get)

      assertTrue(result.exists(_.fold(error => Http4s.report(error).contains("[1]"), _ => false)))
    ,
    test("zero-column output is an encoding failure"):
      val record = CsvDocument.Record(Reference.now(emptyRecord))
      val tupleRows = CsvDocument.Rows(Reference.now(CsvDocument.Tuple(Reference.now(emptyTuple))))
      val body = mixedDsl.body.csv(emptyRecord)
      val requestIssue = Http4sBodyEncoder(Http4sFs2Data.Payload).encode(body, ())

      assertTrue(
        Http4sFs2Data.Payload.encode(record, ()) == Some(Left("A CSV record must have at least one column")),
        Http4sFs2Data.Payload.encode(tupleRows, Vector.empty[Unit]) ==
          Some(Left("A CSV tuple must have at least one column")),
        requestIssue == Left(
          Http4sIssue.Encoding(MediaType("text", "csv"), "A CSV record must have at least one column")
        )
      )
  )
