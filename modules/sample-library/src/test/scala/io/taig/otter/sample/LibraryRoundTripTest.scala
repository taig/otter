package io.taig.otter.sample

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.github.iltotore.iron.autoRefine
import io.taig.otter.http.Http4s
import io.taig.otter.http.Http4sCirce
import io.taig.otter.http.Http4sEnvelope
import io.taig.otter.http.Route
import io.taig.otter.sample.api.BookFilter
import io.taig.otter.sample.api.Borrowed
import io.taig.otter.sample.api.Created
import io.taig.otter.sample.api.Deleted
import io.taig.otter.sample.api.Tracing
import io.taig.otter.sample.api.api
import io.taig.otter.sample.api.books
import io.taig.otter.sample.api.loans
import org.http4s.Request as Http4sRequest
import org.http4s.Uri
import org.http4s.client.Client as Http4sClient
import org.http4s.implicits.*
import scodec.bits.ByteVector
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

import java.time.Clock
import java.time.Instant
import java.time.LocalDate
import java.time.Period
import java.time.ZoneOffset
import java.util.UUID
import scala.collection.immutable.SortedMap

/** Every served endpoint, called the way a caller would call it, and answered by the handler that answers it.
  *
  * The point is that both halves are driven by the *same* endpoint value -- read once as `Endpoint.Server` by the
  * routes and once as `Endpoint.Client` by the caller -- so nothing here can agree by being written twice the same
  * wrong way. A client built by hand against a document could; this cannot.
  *
  * No socket is opened. `Client.fromHttpApp` hands the routes the very request the client interpreter built, which is
  * both faster than binding a port and a stronger claim, since there is no network in between to blame.
  */
object LibraryRoundTripTest extends ZIOSpecDefault:
  private def unwrap[A](value: Either[Problem, A]): IO[A] =
    value.fold(problem => IO.raiseError(new IllegalStateException(problem.title)), IO.pure)

  private val Base: Uri = uri"http://library.test"

  /** Fixed, so a due date is a value a test can write down. */
  private val clock: Clock = Clock.fixed(Instant.parse("2024-06-01T00:00:00Z"), ZoneOffset.UTC)

  private val ada: UUID = UUID.fromString("6f2a5c1e-0b3d-4f7a-9c8e-1d2b3a4c5d6e")

  private val hobbit: Isbn = Isbn.digits("9780261102217")

  private val austen: Isbn = Isbn.digits("9780141439518")

  private val tracing: Tracing = Tracing(requestId = "abc-123", languages = None)

  /** A fresh catalogue per workflow, shared by every endpoint called within it. */
  private def withClient[A](run: Http4s.ApiClient[IO, LibraryRoutes.Payload, Problem] => IO[A]): Task[A] =
    ZIO.fromFuture: _ =>
      Library[IO](Library.State.Seed, clock)
        .flatMap: library =>
          val transport = Http4sClient.fromHttpApp(LibraryRoutes(library))
          val client = Http4s.client(LibraryRoutes.payload, Base, transport).withApi(api.all)
          run(client)
        .unsafeToFuture()

  private val creation: Book.Create = Book.Create(
    isbn = Isbn.digits("9780000000001"),
    title = "A New Book",
    pages = 12,
    genres = List(Genre.Poetry),
    published = LocalDate.of(2024, 1, 1),
    summary = None,
    metadata = SortedMap.empty[String, String]
  )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryRoundTripTest")(
    test("the catalogue serves and decodes its endpoint-local 503 override"):
      val routes = Http4s
        .routes[IO](
          api.all,
          Route(
            books.catalogue,
            (_: Unit) =>
              IO.raiseError[Category](
                new IllegalStateException("catalogue unavailable", new RuntimeException("private catalogue cause"))
              )
          )
        )(Http4sCirce.Payload)
        .orNotFound
      val client = Http4sClient.fromHttpApp(routes)
      ZIO.fromFuture: _ =>
        (for
          response <- routes.run(Http4sRequest[IO](uri = Base / "catalogue"))
          bytes <- Http4sEnvelope.toBytes(response.entity)
          body = bytes.decodeUtf8.getOrElse("")
          answer <- Http4s.client(Http4sCirce.Payload, Base, client).withApi(api.all)(books.catalogue)(())
        yield assertTrue(
          response.status.code == 503,
          answer == Left(Problem.internal),
          body.contains("\"kind\":\"internal\""),
          body.contains("\"detail\":[]"),
          !body.contains("catalogue unavailable"),
          !body.contains("private catalogue cause")
        )).unsafeToFuture()
    ,
    test("a global unexpected failure is an internal problem without exception details"):
      val routes = Http4s
        .routes[IO](
          api.all,
          Route(
            loans.health,
            (_: Unit) =>
              IO.raiseError[Unit](
                new IllegalStateException("health unavailable", new RuntimeException("private health cause"))
              )
          )
        )(Http4sCirce.Payload)
        .orNotFound
      val client = Http4sClient.fromHttpApp(routes)
      ZIO.fromFuture: _ =>
        (for
          response <- routes.run(Http4sRequest[IO](uri = Base / "health"))
          bytes <- Http4sEnvelope.toBytes(response.entity)
          body = bytes.decodeUtf8.getOrElse("")
          answer <- Http4s.client(Http4sCirce.Payload, Base, client).withApi(api.all)(loans.health)(())
        yield assertTrue(
          response.status.code == 500,
          answer == Left(Problem.internal),
          body.contains("\"kind\":\"internal\""),
          body.contains("\"detail\":[]"),
          !body.contains("health unavailable"),
          !body.contains("private health cause")
        )).unsafeToFuture()
    ,
    test("a caller out of step with the server reads an unrouted 404 as its own, and can tell by the kind"):
      val app = Http4s.app[IO](api.all, Route(loans.health, (_: Unit) => IO.unit))(Http4sCirce.Payload)
      val client = Http4sClient.fromHttpApp(app)
      ZIO.fromFuture: _ =>
        Http4s
          .client(Http4sCirce.Payload, Base, client)
          .withApi(api.all)(loans.borrow)(
            (ada, Loan.Request(hobbit, None))
          )
          .map(answer => assertTrue(answer == Right(Borrowed.Unknown(Problem.notFound))))
          .unsafeToFuture()
    ,
    suite("the envelope")(
      test("an answer with no entity round trips as a unit"):
        withClient(client => client(loans.health)(()).flatMap(unwrap)).map(answer => assertTrue(answer == ()))
      ,
      test("a defaulted query reaches the handler as its default, and the whole catalogue comes back"):
        withClient(client =>
          client(books.list)((BookFilter(page = 1, size = 20, genres = Nil, available = false), tracing))
            .flatMap(unwrap)
        ).map(books => assertTrue(books.length == 3))
      ,
      test("a repeated query parameter is read as every value that was given"):
        withClient(client =>
          client(books.list)(
            (BookFilter(page = 1, size = 20, genres = List(Genre.Romance), available = false), tracing)
          ).flatMap(unwrap)
        )
          .map(books => assertTrue(books.map(_.isbn) == List(austen)))
      ,
      test("paging is the caller's, and a size of one yields one"):
        withClient(client =>
          client(books.list)((BookFilter(page = 1, size = 1, genres = Nil, available = false), tracing)).flatMap(unwrap)
        ).map(books => assertTrue(books.length == 1))
      ,
      test("a bare flag is true because the name was given at all"):
        withClient(client =>
          client(books.list)((BookFilter(page = 1, size = 20, genres = Nil, available = true), tracing)).flatMap(unwrap)
        ).map(books => assertTrue(books.length == 3))
      ,
      test("an optional list valued header round trips every value"):
        withClient(client =>
          client(books.list)(
            (
              BookFilter(page = 1, size = 20, genres = Nil, available = false),
              Tracing(requestId = "abc-123", languages = Some(List("en", "de")))
            )
          ).flatMap(unwrap)
        ).map(books => assertTrue(books.length == 3))
      ,
      test("a path placeholder that parses hands the handler the parsed value and not the text"):
        withClient(client => client(books.fetch)(hobbit).flatMap(unwrap)).map(answer =>
          assertTrue(answer.map(_.title) == Some("The Hobbit"))
        )
      ,
      test("a branch with no entity is told apart from one with a body by the status code alone"):
        withClient(client => client(books.fetch)(Isbn.digits("9789999999999")).flatMap(unwrap)).map(answer =>
          assertTrue(answer == None)
        )
    ),
    suite("payloads")(
      test("a book written by the handler is the book the caller reads, refinements and all"):
        withClient(client => client(books.fetch)(hobbit).flatMap(unwrap)).map(answer =>
          assertTrue(
            answer.map(_.pages) == Some(310),
            answer.map(_.genres) == Some(List(Genre.Fantasy, Genre.Children)),
            answer.map(_.published) == Some(LocalDate.of(1937, 9, 21))
          )
        )
      ,
      test("a dictionary round trips under keys nobody named in a schema"):
        withClient(client => client(books.fetch)(hobbit).flatMap(unwrap)).map(answer =>
          assertTrue(answer.map(_.metadata) == Some(SortedMap("condition" -> "good", "shelf" -> "F-TOL")))
        )
      ,
      test("a case insensitive email is one value however it was typed"):
        withClient(client => client(loans.fetch)(ada).flatMap(unwrap)).map(answer =>
          assertTrue(answer.map(_.email.toString) == Some("ada@otter.test"))
        )
      ,
      test("an instant and a local date survive the trip as themselves"):
        withClient(client => client(loans.fetch)(ada).flatMap(unwrap)).map(answer =>
          assertTrue(
            answer.map(_.joined) == Some(Instant.parse("2021-03-04T09:15:00Z")),
            answer.map(_.expires) == Some(LocalDate.of(2027, 3, 4))
          )
        )
      ,
      test("a payload that refers to itself round trips to the depth it was written at"):
        withClient(client => client(books.catalogue)(()).flatMap(unwrap)).map(category =>
          assertTrue(
            category.shelves.length == 2,
            category.shelves.flatMap(_.shelves).map(_.name) == List("Fantasy", "Thriller", "History")
          )
        )
      ,
      test("bytes with no document in them round trip unchanged"):
        val bytes = ByteVector(0x25, 0x50, 0x44, 0x46, 0x00, 0xff)

        withClient(client => client(books.scan)((hobbit, bytes)).flatMap(unwrap)).map(answer =>
          assertTrue(answer == bytes)
        )
    ),
    suite("the status code is chosen by the branch the handler returned")(
      test("a book that is new is created"):
        withClient(client => client(books.create)(creation).flatMap(unwrap)).map(answer =>
          assertTrue(answer == Created.Added(creation.toBook))
        )
      ,
      test("a book that is already there is a conflict, and says which one"):
        withClient { client =>
          for
            _ <- client(books.create)(creation).flatMap(unwrap)
            answer <- client(books.create)(creation).flatMap(unwrap)
          yield answer
        }.map(answer =>
          assertTrue(answer match
            case Created.Duplicate(Problem.Conflict(title, _)) => title.contains("9780000000001")
            case _                                             => false)
        )
      ,
      test("deleting a book nobody is holding answers with no entity at all"):
        withClient(client => client(books.delete)(hobbit).flatMap(unwrap)).map(answer =>
          assertTrue(answer == Deleted.Removed)
        )
      ,
      test("deleting an already removed book still succeeds"):
        withClient { client =>
          for
            _ <- client(books.delete)(hobbit).flatMap(unwrap)
            answer <- client(books.delete)(hobbit).flatMap(unwrap)
          yield answer
        }.map(answer => assertTrue(answer == Deleted.Removed))
      ,
      test("deleting a book somebody is holding is refused, and answers with a document"):
        withClient { client =>
          for
            _ <- client(loans.borrow)((ada, Loan.Request(hobbit, None))).flatMap(unwrap)
            answer <- client(books.delete)(hobbit).flatMap(unwrap)
          yield answer
        }.map(answer =>
          assertTrue(answer match
            case Deleted.Conflict(Problem.Conflict(_, _)) => true
            case _                                        => false)
        )
      ,
      test("a loan is granted, and the period it was granted for is the member's own"):
        withClient(client => client(loans.borrow)((ada, Loan.Request(hobbit, None))).flatMap(unwrap)).map(answer =>
          assertTrue(answer match
            case Borrowed.Lent(loan) =>
              loan.period == Period.ofWeeks(3) &&
              loan.borrowed == LocalDate.of(2024, 6, 1) &&
              loan.due == LocalDate.of(2024, 6, 22)
            case _ => false)
        )
      ,
      test("a period the caller asked for is the period they get, and a Period is calendar arithmetic"):
        withClient(client =>
          client(loans.borrow)((ada, Loan.Request(hobbit, Some(Period.ofMonths(1))))).flatMap(unwrap)
        ).map(answer =>
          assertTrue(answer match
            case Borrowed.Lent(loan) => loan.due == LocalDate.of(2024, 7, 1)
            case _                   => false)
        )
      ,
      test("borrowing for a member nobody has heard of is the third branch and not the second"):
        withClient(client =>
          client(loans.borrow)((UUID.fromString("00000000-0000-4000-8000-000000000000"), Loan.Request(hobbit, None)))
            .flatMap(unwrap)
        )
          .map(answer =>
            assertTrue(answer match
              case Borrowed.Unknown(_) => true
              case _                   => false)
          )
      ,
      test("borrowing a book somebody else has is the conflict branch"):
        withClient { client =>
          for
            _ <- client(loans.borrow)((ada, Loan.Request(hobbit, None))).flatMap(unwrap)
            answer <- client(loans.borrow)((ada, Loan.Request(hobbit, None))).flatMap(unwrap)
          yield answer
        }
          .map(answer =>
            assertTrue(answer match
              case Borrowed.Unavailable(_) => true
              case _                       => false)
          )
    ),
    suite("content negotiation")(
      test("the alternative the caller wrote is the one the handler reads"):
        withClient(client => client(books.intake)(Some(Left(creation))).flatMap(unwrap)).map(answer =>
          assertTrue(answer == ())
        )
      ,
      test("the other alternative is told apart by its media type and not by parsing"):
        withClient(client => client(books.intake)(Some(Right(ByteVector(0x25, 0x50, 0x44, 0x46)))).flatMap(unwrap)).map(
          answer => assertTrue(answer == ())
        )
      ,
      test("a body that need not be sent, and was not, reaches the handler as nothing at all"):
        withClient(client => client(books.intake)(None).flatMap(unwrap)).map(answer => assertTrue(answer == ()))
    ),
    suite("a field that may be absent, and a field that may be null")(
      test("a key left out leaves the value as it was"):
        withClient { client =>
          for
            _ <- client(books.patch)((austen, Book.Patch(Some("Pride"), None, None, None))).flatMap(unwrap)
            answer <- client(books.fetch)(austen).flatMap(unwrap)
          yield answer
        }
          .map(answer => assertTrue(answer.map(_.title) == Some("Pride"), answer.map(_.pages) == Some(432)))
      ,
      test("an explicit null is a different thing from a missing key, and clears the value"):
        withClient { client =>
          for
            _ <- client(books.patch)((hobbit, Book.Patch(None, None, None, Some(None)))).flatMap(unwrap)
            answer <- client(books.fetch)(hobbit).flatMap(unwrap)
          yield answer
        }.map(answer => assertTrue(answer.map(_.summary) == Some(None)))
      ,
      test("a book whose summary was never set reads back as having none"):
        withClient(client => client(books.fetch)(austen).flatMap(unwrap)).map(answer =>
          assertTrue(answer.map(_.summary) == Some(None))
        )
    )
  )
