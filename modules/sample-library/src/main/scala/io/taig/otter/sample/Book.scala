package io.taig.otter.sample

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import java.time.LocalDate
import scala.collection.immutable.SortedMap

/** A book as the catalogue holds it.
  *
  * The refinements are on the fields rather than checked in a handler, which is the whole of what `core-iron` buys: a
  * `Book` that exists has a title of between one and two hundred characters and a positive page count, because a
  * decoder that could not prove it never produced one. Nothing here re-checks them.
  */
final case class Book(
    isbn: Isbn,
    title: Book.Title,
    pages: Book.Pages,
    genres: List[Genre],
    published: LocalDate,
    summary: Option[String],
    metadata: SortedMap[String, String]
)

object Book:
  /** Between one and two hundred characters: a book with no title is not a book, and a title longer than this is a
    * summary that arrived in the wrong field.
    */
  type Title = String :| (MinLength[1] & MaxLength[200])

  /** A book has at least one page. */
  type Pages = Int :| Positive

  /** At most ten, which is a cataloguing rule and not a fact about books.
    *
    * A collection constraint rather than one on an element, which is the third of the three vocabularies
    * [[io.taig.otter.component.IronComponent]] offers and the one that says something about the container.
    */
  type Genres = List[Genre] :| MaxLength[10]

  /** What a caller sends to add one, which is the whole book minus what the catalogue decides.
    *
    * A type of its own rather than a `Book` with optional fields, because the two genuinely differ: `metadata` is
    * defaulted here and always present there, and an endpoint that took a `Book` would be asking a caller for values it
    * is not in a position to know.
    */
  final case class Create(
      isbn: Isbn,
      title: Book.Title,
      pages: Book.Pages,
      genres: Book.Genres,
      published: LocalDate,
      summary: Option[String],
      metadata: SortedMap[String, String]
  ):
    def toBook: Book = Book(isbn, title, pages, genres, published, summary, metadata)

  /** What a caller sends to change one.
    *
    * Every field is absent-able, and the two kinds of absence are told apart on purpose: a missing key means "leave it
    * as it is", and an explicit `null` in `summary` means "there is no summary". That distinction is the reason
    * `.optional.nullable` exists, and it is the clearest case in this API of a schema whose read and write sides
    * differ.
    */
  final case class Patch(
      title: Option[Book.Title],
      pages: Option[Book.Pages],
      genres: Option[Book.Genres],
      summary: Option[Option[String]]
  ):
    def apply(book: Book): Book = book.copy(
      title = title.getOrElse(book.title),
      pages = pages.getOrElse(book.pages),
      genres = genres.getOrElse(book.genres),
      summary = summary.getOrElse(book.summary)
    )

  /** One book, flattened until it fits a row.
    *
    * A type of its own because a flat format cannot hold the book: `genres` is a list and `metadata` a dictionary, and
    * neither fits in a cell. Deciding what to drop is the caller's, not a renderer's, which is why this is written down
    * rather than derived from [[Book]].
    */
  final case class Row(isbn: Isbn, title: String, pages: Int, published: LocalDate)

  object Row:
    def apply(book: Book): Book.Row = Book.Row(book.isbn, book.title, book.pages, book.published)

  /** The catalogue a fresh server starts with. */
  val Seed: List[Book] = List(
    Book(
      isbn = Isbn.digits("9780261102217"),
      title = "The Hobbit",
      pages = 310,
      genres = List(Genre.Fantasy, Genre.Children),
      published = LocalDate.of(1937, 9, 21),
      summary = Some("A hobbit is talked into a burglary."),
      metadata = SortedMap("shelf" -> "F-TOL", "condition" -> "good")
    ),
    Book(
      isbn = Isbn.digits("9780141439518"),
      title = "Pride and Prejudice",
      pages = 432,
      genres = List(Genre.Romance),
      published = LocalDate.of(1813, 1, 28),
      summary = None,
      metadata = SortedMap("shelf" -> "R-AUS")
    ),
    Book(
      isbn = Isbn.digits("9780241265543"),
      title = "A Short History of Nearly Everything",
      pages = 687,
      genres = List(Genre.History, Genre.Biography),
      published = LocalDate.of(2003, 5, 6),
      summary = Some("Most of what there is, briefly."),
      metadata = SortedMap.empty
    )
  )
