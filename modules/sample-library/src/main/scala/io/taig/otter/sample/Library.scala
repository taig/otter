package io.taig.otter.sample

import cats.effect.Ref
import cats.effect.Sync
import cats.syntax.all.*
import io.taig.otter.sample.api.BookFilter
import io.taig.otter.sample.api.Borrowed
import io.taig.otter.sample.api.Created
import io.taig.otter.sample.api.Deleted
import io.taig.otter.sample.api.Uploaded
import scodec.bits.ByteVector

import java.time.Clock
import java.time.LocalDate
import java.util.UUID
import scala.collection.immutable.SortedMap

/** What answers the endpoints.
  *
  * A `Ref` and nothing else. There is no repository trait, no database and no service layer, because none of them would
  * be showing anything about this library -- and every indirection between an endpoint and the value it answers with is
  * one more thing a reader has to hold before the interesting part.
  *
  * Every method here is exactly `A => F[B]` for the `A` and `B` its endpoint named. That is the whole contract
  * [[io.taig.otter.http.Route]] asks for: no request object to reach into, no response builder to get wrong, and no
  * status code written down anywhere -- which one is sent follows from which branch of the answer type came back.
  */
final class Library[F[_]: Sync](state: Ref[F, Library.State], clock: Clock):
  private def today: F[LocalDate] = Sync[F].delay(LocalDate.now(clock))

  private def reference: F[UUID] = Sync[F].delay(UUID.randomUUID())

  /** Paged, filtered and sorted by ISBN, which is the order the catalogue is kept in. */
  def list(filter: BookFilter): F[List[Book]] =
    state.get.map: current =>
      val offset = (filter.page - 1).max(0) * filter.size
      current.books.values.toList
        .filter(book => filter.genres.isEmpty || filter.genres.exists(book.genres.contains))
        .filter(book => !filter.available || !current.loans.values.exists(_.isbn == book.isbn))
        .sortBy(_.isbn.value)
        .slice(offset, offset + filter.size)

  /** One immutable snapshot, traversed lazily in its existing ISBN order. */
  def exported: F[fs2.Stream[F, Book]] =
    state.get.map(current => fs2.Stream.fromIterator[F](current.books.valuesIterator, 64))

  def create(create: Book.Create): F[Created] = state.modify: current =>
    if current.books.contains(create.isbn) then
      (current, Created.Duplicate(Problem.conflict(s"${create.isbn.value} is already in the catalogue")))
    else
      val book = create.toBook
      (current.copy(books = current.books.updated(book.isbn, book)), Created.Added(book))

  def fetch(isbn: Isbn): F[Option[Book]] = state.get.map(_.books.get(isbn))

  def patch(isbn: Isbn, patch: Book.Patch): F[Option[Book]] = state.modify: current =>
    current.books.get(isbn) match
      case None       => (current, None)
      case Some(book) =>
        val patched = patch(book)
        (current.copy(books = current.books.updated(isbn, patched)), Some(patched))

  /** Idempotent: a book that is not there is already gone. A book somebody is holding cannot be removed at all. */
  def delete(isbn: Isbn): F[Deleted] = state.modify: current =>
    if current.loans.values.exists(_.isbn == isbn) then
      (current, Deleted.Conflict(Problem.conflict(s"${isbn.value} is on loan")))
    else (current.copy(books = current.books.removed(isbn), covers = current.covers.removed(isbn)), Deleted.Removed)

  def cover(isbn: Isbn): F[Option[ByteVector]] = state.get.map(_.covers.get(isbn))

  def upload(isbn: Isbn, cover: (Book.Patch, Option[ByteVector])): F[Uploaded] = state.modify: current =>
    current.books.get(isbn) match
      case None       => (current, Uploaded.Missing(Problem.missing(s"${isbn.value} is not in the catalogue")))
      case Some(book) =>
        val (patch, image) = cover
        val covers = image.fold(current.covers)(bytes => current.covers.updated(isbn, bytes))
        (current.copy(books = current.books.updated(isbn, patch(book)), covers = covers), Uploaded.Stored)

  /** Bytes in, bytes out. Nothing is stored, because what a scan *is* is not this sample's subject.
    *
    * The ISBN is a parameter because the endpoint's path holds one, and a handler is exactly `A => F[B]` for the `A`
    * its endpoint named -- there is no request object to take only part of.
    */
  def scan(@annotation.unused isbn: Isbn, bytes: ByteVector): F[ByteVector] = Sync[F].pure(bytes)

  /** Either alternative is accepted and acknowledged, and so is neither.
    *
    * Which one arrived was decided by the media type, and whether one arrived at all by there being an entity. A notice
    * with nothing attached is a shipment announced and not yet catalogued.
    */
  def intake(submission: Option[Either[Book.Create, ByteVector]]): F[Unit] = submission match
    case Some(Left(create)) => Sync[F].void(this.create(create))
    case Some(Right(_))     => Sync[F].unit
    case None               => Sync[F].unit

  def catalogue: F[Category] = Sync[F].pure(Category.Root)

  def member(reference: UUID): F[Option[Member]] = state.get.map(_.members.get(reference))

  /** The one handler with a rule in it, and the rule is where `java.time` earns its place: a loan period is a `Period`
    * and not a number of days, so adding it to a date is calendar arithmetic rather than counting.
    */
  def borrow(member: UUID, request: Loan.Request): F[Borrowed] =
    (this.today, this.reference).tupled.flatMap: (today, loan) =>
      state.modify: current =>
        current.members.get(member) match
          case None => (current, Borrowed.Unknown(Problem.missing(s"no member $member")))
          case Some(_) if !current.books.contains(request.isbn) =>
            (current, Borrowed.Unknown(Problem.missing(s"${request.isbn.value} is not in the catalogue")))
          case Some(_) if current.loans.values.exists(_.isbn == request.isbn) =>
            (current, Borrowed.Unavailable(Problem.conflict(s"${request.isbn.value} is already on loan")))
          case Some(found) =>
            val period = request.period.getOrElse(found.membership.loanPeriod)
            val lent = Loan(loan, request.isbn, member, today, period, today.plus(period))

            (current.copy(loans = current.loans.updated(loan, lent)), Borrowed.Lent(lent))

  def health: F[Unit] = Sync[F].unit

object Library:
  final case class State(
      books: SortedMap[Isbn, Book],
      members: SortedMap[UUID, Member],
      loans: SortedMap[UUID, Loan],
      covers: SortedMap[Isbn, ByteVector] = SortedMap.empty
  )

  object State:
    /** The catalogue a fresh server starts with. */
    val Seed: Library.State = Library.State(
      books = SortedMap.from(Book.Seed.map(book => book.isbn -> book)),
      members = SortedMap.from(Member.Seed.map(member => member.reference -> member)),
      loans = SortedMap.empty
    )

  def apply[F[_]: Sync](state: Library.State = Library.State.Seed, clock: Clock = Clock.systemUTC): F[Library[F]] =
    Ref[F].of(state).map(new Library[F](_, clock))
