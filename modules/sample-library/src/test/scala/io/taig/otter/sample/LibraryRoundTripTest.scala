package io.taig.otter.sample

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.github.iltotore.iron.autoRefine
import io.taig.otter.http.ComposedEndpoint
import io.taig.otter.http.Http4s
import io.taig.otter.http.Http4sCirce
import io.taig.otter.sample.api.Borrowed
import io.taig.otter.sample.api.Created
import io.taig.otter.sample.api.contract
import io.taig.otter.sample.api.dsl
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

  /** A fresh catalogue per call, so no test can see what another one wrote. */
  private def call[A, B](endpoint: ComposedEndpoint[dsl.Payload, A, A, B, B, Problem])(value: A): Task[B] =
    ZIO.fromFuture: _ =>
      Library[IO](Library.State.Seed, clock)
        .flatMap: library =>
          val client = Http4sClient.fromHttpApp(LibraryRoutes(library).orNotFound)

          Http4s
            .client[IO, A, Either[Problem, B]](Http4sCirce.Payload, Base, client)(endpoint.effective)(value)
            .flatMap(unwrap)
        .unsafeToFuture()

  /** Two calls against one catalogue, for the claims that need a server to remember something. */
  private def calls[A1, B1, A2, B2](
      first: ComposedEndpoint[dsl.Payload, A1, A1, B1, B1, Problem],
      second: ComposedEndpoint[dsl.Payload, A2, A2, B2, B2, Problem]
  )(a1: A1, a2: A2): Task[B2] =
    ZIO.fromFuture: _ =>
      Library[IO](Library.State.Seed, clock)
        .flatMap: library =>
          val client = Http4sClient.fromHttpApp(LibraryRoutes(library).orNotFound)

          Http4s
            .client[IO, A1, Either[Problem, B1]](Http4sCirce.Payload, Base, client)(first.effective)(a1)
            .flatMap(unwrap) *>
            Http4s
              .client[IO, A2, Either[Problem, B2]](Http4sCirce.Payload, Base, client)(second.effective)(a2)
              .flatMap(unwrap)
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
    suite("the envelope")(
      test("an answer with no entity round trips as a unit"):
        call(contract.health)(()).map(answer => assertTrue(answer == ()))
      ,
      test("a defaulted query reaches the handler as its default, and the whole catalogue comes back"):
        call(contract.listBooks)((1, 20, Nil, false, ("abc-123", None))).map(books => assertTrue(books.length == 3))
      ,
      test("a repeated query parameter is read as every value that was given"):
        call(contract.listBooks)((1, 20, List(Genre.Romance), false, ("abc-123", None))).map(books =>
          assertTrue(books.map(_.isbn) == List(austen))
        )
      ,
      test("paging is the caller's, and a size of one yields one"):
        call(contract.listBooks)((1, 1, Nil, false, ("abc-123", None))).map(books => assertTrue(books.length == 1))
      ,
      test("a bare flag is true because the name was given at all"):
        call(contract.listBooks)((1, 20, Nil, true, ("abc-123", None))).map(books => assertTrue(books.length == 3))
      ,
      test("an optional list valued header round trips every value"):
        call(contract.listBooks)((1, 20, Nil, false, ("abc-123", Some(List("en", "de"))))).map(books =>
          assertTrue(books.length == 3)
        )
      ,
      test("a path placeholder that parses hands the handler the parsed value and not the text"):
        call(contract.fetchBook)(hobbit).map(answer => assertTrue(answer.map(_.title) == Some("The Hobbit")))
      ,
      test("a branch with no entity is told apart from one with a body by the status code alone"):
        call(contract.fetchBook)(Isbn.digits("9789999999999")).map(answer => assertTrue(answer == None))
    ),
    suite("payloads")(
      test("a book written by the handler is the book the caller reads, refinements and all"):
        call(contract.fetchBook)(hobbit).map(answer =>
          assertTrue(
            answer.map(_.pages) == Some(310),
            answer.map(_.genres) == Some(List(Genre.Fantasy, Genre.Children)),
            answer.map(_.published) == Some(LocalDate.of(1937, 9, 21))
          )
        )
      ,
      test("a dictionary round trips under keys nobody named in a schema"):
        call(contract.fetchBook)(hobbit).map(answer =>
          assertTrue(answer.map(_.metadata) == Some(SortedMap("condition" -> "good", "shelf" -> "F-TOL")))
        )
      ,
      test("a case insensitive email is one value however it was typed"):
        call(contract.fetchLoans)(ada).map(answer => assertTrue(answer.map(_.email.toString) == Some("ada@otter.test")))
      ,
      test("an instant and a local date survive the trip as themselves"):
        call(contract.fetchLoans)(ada).map(answer =>
          assertTrue(
            answer.map(_.joined) == Some(Instant.parse("2021-03-04T09:15:00Z")),
            answer.map(_.expires) == Some(LocalDate.of(2027, 3, 4))
          )
        )
      ,
      test("a payload that refers to itself round trips to the depth it was written at"):
        call(contract.catalogue)(()).map(category =>
          assertTrue(
            category.shelves.length == 2,
            category.shelves.flatMap(_.shelves).map(_.name) == List("Fantasy", "Thriller", "History")
          )
        )
      ,
      test("bytes with no document in them round trip unchanged"):
        val bytes = ByteVector(0x25, 0x50, 0x44, 0x46, 0x00, 0xff)

        call(contract.scanBooks)((hobbit, bytes)).map(answer => assertTrue(answer == bytes))
    ),
    suite("the status code is chosen by the branch the handler returned")(
      test("a book that is new is created"):
        call(contract.createBook)(creation).map(answer => assertTrue(answer == Created.Added(creation.toBook)))
      ,
      test("a book that is already there is a conflict, and says which one"):
        calls(contract.createBook, contract.createBook)(creation, creation).map(answer =>
          assertTrue(answer match
            case Created.Duplicate(problem) =>
              problem.kind == Problem.Kind.Conflict && problem.title.contains("9780000000001")
            case _ => false)
        )
      ,
      test("deleting a book nobody is holding answers with no entity at all"):
        call(contract.deleteBook)(hobbit).map(answer => assertTrue(answer == Left(())))
      ,
      test("deleting a book somebody is holding is refused, and answers with a document"):
        calls(contract.borrow, contract.deleteBook)((ada, Loan.Request(hobbit, None)), hobbit).map(answer =>
          assertTrue(answer.toOption.map(_.kind) == Some(Problem.Kind.Conflict))
        )
      ,
      test("a loan is granted, and the period it was granted for is the member's own"):
        call(contract.borrow)((ada, Loan.Request(hobbit, None))).map(answer =>
          assertTrue(answer match
            case Borrowed.Lent(loan) =>
              loan.period == Period.ofWeeks(3) &&
              loan.borrowed == LocalDate.of(2024, 6, 1) &&
              loan.due == LocalDate.of(2024, 6, 22)
            case _ => false)
        )
      ,
      test("a period the caller asked for is the period they get, and a Period is calendar arithmetic"):
        call(contract.borrow)((ada, Loan.Request(hobbit, Some(Period.ofMonths(1))))).map(answer =>
          assertTrue(answer match
            case Borrowed.Lent(loan) => loan.due == LocalDate.of(2024, 7, 1)
            case _                   => false)
        )
      ,
      test("borrowing for a member nobody has heard of is the third branch and not the second"):
        call(contract.borrow)((UUID.fromString("00000000-0000-4000-8000-000000000000"), Loan.Request(hobbit, None)))
          .map(answer =>
            assertTrue(answer match
              case Borrowed.Unknown(_) => true
              case _                   => false)
          )
      ,
      test("borrowing a book somebody else has is the conflict branch"):
        calls(contract.borrow, contract.borrow)((ada, Loan.Request(hobbit, None)), (ada, Loan.Request(hobbit, None)))
          .map(answer =>
            assertTrue(answer match
              case Borrowed.Unavailable(_) => true
              case _                       => false)
          )
    ),
    suite("content negotiation")(
      test("the alternative the caller wrote is the one the handler reads"):
        call(contract.intakeBooks)(Some(Left(creation))).map(answer => assertTrue(answer == ()))
      ,
      test("the other alternative is told apart by its media type and not by parsing"):
        call(contract.intakeBooks)(Some(Right(ByteVector(0x25, 0x50, 0x44, 0x46)))).map(answer =>
          assertTrue(answer == ())
        )
      ,
      test("a body that need not be sent, and was not, reaches the handler as nothing at all"):
        call(contract.intakeBooks)(None).map(answer => assertTrue(answer == ()))
    ),
    suite("a field that may be absent, and a field that may be null")(
      test("a key left out leaves the value as it was"):
        calls(contract.patchBook, contract.fetchBook)((austen, Book.Patch(Some("Pride"), None, None, None)), austen)
          .map(answer => assertTrue(answer.map(_.title) == Some("Pride"), answer.map(_.pages) == Some(432)))
      ,
      test("an explicit null is a different thing from a missing key, and clears the value"):
        calls(contract.patchBook, contract.fetchBook)((hobbit, Book.Patch(None, None, None, Some(None))), hobbit).map(
          answer => assertTrue(answer.map(_.summary) == Some(None))
        )
      ,
      test("a book whose summary was never set reads back as having none"):
        call(contract.fetchBook)(austen).map(answer => assertTrue(answer.map(_.summary) == Some(None)))
    )
  )
