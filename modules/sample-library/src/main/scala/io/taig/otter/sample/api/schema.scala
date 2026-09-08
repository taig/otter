package io.taig.otter.sample.api

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.taig.otter.Csv
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.sample.Book
import io.taig.otter.sample.Category
import io.taig.otter.sample.Genre
import io.taig.otter.sample.Isbn
import io.taig.otter.sample.Loan
import io.taig.otter.sample.Member
import io.taig.otter.sample.Membership
import io.taig.otter.sample.Problem
import io.taig.otter.syntax.JsonSchemaSyntax.*
import io.taig.otter.syntax.JsonSyntax.*
import io.taig.otter.syntax.all.*

import scala.collection.immutable.SortedMap

/** Every document this API reads or writes.
  *
  * Named schemas -- the ones carrying [[io.taig.otter.Keys.name]] -- are declared once under `components/schemas` and
  * referred to by `$ref` from everywhere they are used, which is also the only way a schema that refers to itself can
  * be written down: a reference needs something to point at.
  */
object schema:
  /** Text on the wire, an [[Isbn]] in Scala, and a round trip rather than a one way read: `codec` is what says a value
    * can be written back, where `parser` would leave a schema that reads but cannot answer.
    */
  val isbn: Json.Primitive.Text[Isbn] = payload.codec("isbn", Isbn.parse, _.value)

  /** A closed set, matched exhaustively. Adding a case to [[Genre]] fails to compile here rather than failing to read
    * at runtime.
    */
  val genre: Json.Enumeration[Genre] = payload
    .enumeration[Json.Primitive.Text.Schema, String, Genre](payload.string):
      case Genre.Biography => "biography"
      case Genre.Children  => "children"
      case Genre.Fantasy   => "fantasy"
      case Genre.History   => "history"
      case Genre.Poetry    => "poetry"
      case Genre.Romance   => "romance"
      case Genre.Thriller  => "thriller"
    .attr(Keys.name, "Genre")

  val membership: Json.Enumeration[Membership] = payload
    .enumeration[Json.Primitive.Text.Schema, String, Membership](payload.string):
      case Membership.Standard => "standard"
      case Membership.Student  => "student"
      case Membership.Staff    => "staff"
    .attr(Keys.name, "Membership")

  /** Free form strings under free form keys, which is what a dictionary is for: a record names its members and this
    * cannot, because whoever fills it in decides what they are called.
    */
  val metadata: Json.Dictionary[SortedMap[String, String]] =
    payload.dictionary.map(payload.string)

  /** Held under an ISBN, which is text on the wire like every key -- there are no numeric keys in JSON, so a key that
    * carries something other than a string is spelled as a `codec` and not as that thing's own primitive.
    */
  val fines: Json.Dictionary[SortedMap[String, BigDecimal]] =
    payload.dictionary.map(payload.string, payload.bigDecimal)

  /** The refinements are the field types, not a check a handler runs: a `Book` that exists has a title of the right
    * length because no decoder produced one that did not.
    */
  val book: Json.Record[Book] = (
    payload.field("isbn", schema.isbn).description("The thirteen digit ISBN, hyphens optional") :*
      payload.field("title", payload.refined.string[MinLength[1] & MaxLength[200]]) :*
      payload.field("pages", payload.refined.int[Positive]).examples(310) :*
      payload.field("genres", payload.collection.list(schema.genre)) :*
      payload.field("published", payload.localDate) :*
      payload.field("summary", payload.string).optional.nullable :*
      payload.field("metadata", schema.metadata)
  ).to[Book]
    .attr(Keys.name, "Book")
    .title("Book")
    .description("A book as the catalogue holds it")

  /** The same members, with `metadata` defaulted.
    *
    * This is the case where the two sides of one schema genuinely differ, and the reason a document is rendered per
    * side: a reader accepts the key's absence, a writer always produces it, and a caller reading the server's document
    * must not be told the field is required.
    */
  val create: Json.Record[Book.Create] = (
    payload.field("isbn", schema.isbn) :*
      payload.field("title", payload.refined.string[MinLength[1] & MaxLength[200]]) :*
      payload.field("pages", payload.refined.int[Positive]) :*
      payload
        .field("genres", payload.refined.list[MaxLength[10]](schema.genre))
        .optional(List.empty[Genre].assume[MaxLength[10]]) :*
      payload.field("published", payload.localDate) :*
      payload.field("summary", payload.string).optional.nullable :*
      payload.field("metadata", schema.metadata).optional(SortedMap.empty[String, String])
  ).to[Book.Create].attr(Keys.name, "BookCreate")

  /** Two layers of absence, and only a strict field tells them apart: no `summary` key at all means leave it, and an
    * explicit `null` means there is none. That is `Option[Option[String]]`, and it is why `.omitted.strict` is here
    * rather than the plain `.optional` every other field gets.
    */
  val patch: Json.Record[Book.Patch] = (
    payload.field("title", payload.refined.string[MinLength[1] & MaxLength[200]]).optional :*
      payload.field("pages", payload.refined.int[Positive]).optional :*
      payload.field("genres", payload.refined.list[MaxLength[10]](schema.genre)).optional :*
      payload.field("summary", payload.string.optional).optional.omitted.strict
  ).to[Book.Patch].attr(Keys.name, "BookPatch")

  /** A schema that refers to itself, which works only because it is named.
    *
    * `lazy` because the reference is forward: a `val` would capture it before it is initialised. A collection only
    * forces its element schema once it has an element, so nothing is evaluated until a document is actually read.
    */
  lazy val category: Json.Record[Category] = (
    payload.field("name", payload.string) :*
      payload.field("shelves", payload.collection.list(schema.category)) :*
      payload.field("holdings", payload.int)
  ).to[Category].attr(Keys.name, "Category")

  val member: Json.Record[Member] = (
    payload.field("reference", payload.uuid) :*
      payload.field("email", payload.ciString).description("Matched without regard to case") :*
      payload.field("joined", payload.instant) :*
      payload.field("membership", schema.membership) :*
      payload.field("expires", payload.localDate) :*
      payload.field("fines", schema.fines)
  ).to[Member].attr(Keys.name, "Member")

  val loan: Json.Record[Loan] = (
    payload.field("reference", payload.uuid) :*
      payload.field("isbn", schema.isbn) :*
      payload.field("member", payload.uuid) :*
      payload.field("borrowed", payload.localDate) :*
      payload.field("period", payload.period).description("ISO-8601, so P3W and P21D are different spans") :*
      payload.field("due", payload.localDate)
  ).to[Loan].attr(Keys.name, "Loan")

  /** `period` may be left out and is then the member's own, which the *server* knows and a caller does not. */
  val borrow: Json.Record[Loan.Request] = (
    payload.field("isbn", schema.isbn) :*
      payload.field("period", payload.period).optional
  ).to[Loan.Request].attr(Keys.name, "BorrowRequest")

  val kind: Json.Enumeration[Problem.Kind] = payload
    .enumeration[Json.Primitive.Text.Schema, String, Problem.Kind](payload.string):
      case Problem.Kind.Malformed => "malformed"
      case Problem.Kind.Conflict  => "conflict"
      case Problem.Kind.Missing   => "missing"
    .attr(Keys.name, "ProblemKind")

  /** One error shape for the whole API, which is what makes passing it to `Http4s.routes` worth doing: a caller reads
    * the same document whether the request broke the schema or the handler refused it.
    */
  val problem: Json.Record[Problem] = (
    payload.field("kind", schema.kind) :*
      payload.field("title", payload.string) :*
      payload.field("detail", payload.collection.list(payload.string)).optional(Nil)
  ).to[Problem]
    .attr(Keys.name, "Problem")
    .description("What this API says when it cannot say what was asked for")

  /** One line of the CSV export.
    *
    * A different alphabet, and a smaller one: a row's members are all cells, so there is no `branch`, no `collection`
    * and no `dictionary` here -- none of them fit in a cell. Nothing in this repository interprets a CSV body, which is
    * the point of writing one down: an endpoint may describe a payload in any alphabet at all, and what cannot be
    * carried is reported rather than quietly dropped.
    */
  val row: Csv[Book.Row] = (
    rows.field("isbn", rows.codec("isbn", Isbn.parse, _.value)) :*
      rows.field("title", rows.string) :*
      rows.field("pages", rows.int) :*
      rows.field("published", rows.localDate)
  ).to[Book.Row]
