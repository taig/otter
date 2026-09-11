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
  val isbn: Json.Primitive.Text[Isbn] = json.codec("isbn", Isbn.parse, _.value)

  /** A closed set, matched exhaustively. Adding a case to [[Genre]] fails to compile here rather than failing to read
    * at runtime.
    */
  val genre: Json.Enumeration[Genre] = json
    .enumeration[Json.Primitive.Text.Schema, String, Genre](json.string):
      case Genre.Biography => "biography"
      case Genre.Children  => "children"
      case Genre.Fantasy   => "fantasy"
      case Genre.History   => "history"
      case Genre.Poetry    => "poetry"
      case Genre.Romance   => "romance"
      case Genre.Thriller  => "thriller"
    .attr(Keys.name, "Genre")

  val membership: Json.Enumeration[Membership] = json
    .enumeration[Json.Primitive.Text.Schema, String, Membership](json.string):
      case Membership.Standard => "standard"
      case Membership.Student  => "student"
      case Membership.Staff    => "staff"
    .attr(Keys.name, "Membership")

  /** Free form strings under free form keys, which is what a dictionary is for: a record names its members and this
    * cannot, because whoever fills it in decides what they are called.
    */
  val metadata: Json.Dictionary[SortedMap[String, String]] =
    json.dictionary.map(json.string)

  /** Held under an ISBN, which is text on the wire like every key -- there are no numeric keys in JSON, so a key that
    * carries something other than a string is spelled as a `codec` and not as that thing's own primitive.
    */
  val fines: Json.Dictionary[SortedMap[String, BigDecimal]] =
    json.dictionary.map(json.string, json.bigDecimal)

  /** The refinements are the field types, not a check a handler runs: a `Book` that exists has a title of the right
    * length because no decoder produced one that did not.
    */
  val book: Json.Record[Book] = (
    json.field("isbn", schema.isbn).description("The thirteen digit ISBN, hyphens optional") :*
      json.field("title", json.refined.string[MinLength[1] & MaxLength[200]]) :*
      json.field("pages", json.refined.int[Positive]).examples(310) :*
      json.field("genres", json.collection.list(schema.genre)) :*
      json.field("published", json.localDate) :*
      json.field("summary", json.string).optional.nullable :*
      json.field("metadata", schema.metadata)
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
    json.field("isbn", schema.isbn) :*
      json.field("title", json.refined.string[MinLength[1] & MaxLength[200]]) :*
      json.field("pages", json.refined.int[Positive]) :*
      json
        .field("genres", json.refined.list[MaxLength[10]](schema.genre))
        .optional(List.empty[Genre].assume[MaxLength[10]]) :*
      json.field("published", json.localDate) :*
      json.field("summary", json.string).optional.nullable :*
      json.field("metadata", schema.metadata).optional(SortedMap.empty[String, String])
  ).to[Book.Create].attr(Keys.name, "BookCreate")

  /** Two layers of absence, and only a strict field tells them apart: no `summary` key at all means leave it, and an
    * explicit `null` means there is none. That is `Option[Option[String]]`, and it is why `.omitted.strict` is here
    * rather than the plain `.optional` every other field gets.
    */
  val patch: Json.Record[Book.Patch] = (
    json.field("title", json.refined.string[MinLength[1] & MaxLength[200]]).optional :*
      json.field("pages", json.refined.int[Positive]).optional :*
      json.field("genres", json.refined.list[MaxLength[10]](schema.genre)).optional :*
      json.field("summary", json.string.optional).optional.omitted.strict
  ).to[Book.Patch].attr(Keys.name, "BookPatch")

  /** A schema that refers to itself, which works only because it is named.
    *
    * `lazy` because the reference is forward: a `val` would capture it before it is initialised. A collection only
    * forces its element schema once it has an element, so nothing is evaluated until a document is actually read.
    */
  lazy val category: Json.Record[Category] = (
    json.field("name", json.string) :*
      json.field("shelves", json.collection.list(schema.category)) :*
      json.field("holdings", json.int)
  ).to[Category].attr(Keys.name, "Category")

  val member: Json.Record[Member] = (
    json.field("reference", json.uuid) :*
      json.field("email", json.ciString).description("Matched without regard to case") :*
      json.field("joined", json.instant) :*
      json.field("membership", schema.membership) :*
      json.field("expires", json.localDate) :*
      json.field("fines", schema.fines)
  ).to[Member].attr(Keys.name, "Member")

  val loan: Json.Record[Loan] = (
    json.field("reference", json.uuid) :*
      json.field("isbn", schema.isbn) :*
      json.field("member", json.uuid) :*
      json.field("borrowed", json.localDate) :*
      json.field("period", json.period).description("ISO-8601, so P3W and P21D are different spans") :*
      json.field("due", json.localDate)
  ).to[Loan].attr(Keys.name, "Loan")

  /** `period` may be left out and is then the member's own, which the *server* knows and a caller does not. */
  val borrow: Json.Record[Loan.Request] = (
    json.field("isbn", schema.isbn) :*
      json.field("period", json.period).optional
  ).to[Loan.Request].attr(Keys.name, "BorrowRequest")

  val kind: Json.Enumeration[Problem.Kind] = json
    .enumeration[Json.Primitive.Text.Schema, String, Problem.Kind](json.string):
      case Problem.Kind.Malformed => "malformed"
      case Problem.Kind.Conflict  => "conflict"
      case Problem.Kind.Missing   => "missing"
    .attr(Keys.name, "ProblemKind")

  /** One error shape for the whole API, which is what makes passing it to `Http4s.routes` worth doing: a caller reads
    * the same document whether the request broke the schema or the handler refused it.
    */
  val problem: Json.Record[Problem] = (
    json.field("kind", schema.kind) :*
      json.field("title", json.string) :*
      json.field("detail", json.collection.list(json.string)).optional(Nil)
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
    csv.field("isbn", csv.codec("isbn", Isbn.parse, _.value)) :*
      csv.field("title", csv.string) :*
      csv.field("pages", csv.int) :*
      csv.field("published", csv.localDate)
  ).to[Book.Row]
