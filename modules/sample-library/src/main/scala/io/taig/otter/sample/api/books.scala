package io.taig.otter.sample.api

import io.taig.otter.Keys
import io.taig.otter.http.Bodies
import io.taig.otter.http.Body
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Frame
import io.taig.otter.http.Headers
import io.taig.otter.http.Multipart
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.http.Queries
import io.taig.otter.sample.Book
import io.taig.otter.sample.Category
import io.taig.otter.sample.Genre
import io.taig.otter.sample.Isbn
import io.taig.otter.sample.Problem
import io.taig.otter.sample.api.dsl.*
import scodec.bits.ByteVector

/** What answers `POST /books`: the book, or the reason it is already there.
  *
  * A named sum rather than the `Either[Book, Problem]` the union builds, because a handler returning
  * `Created.Duplicate(problem)` says what it means where `Right(problem)` does not. `.to` maps one onto the other, so
  * the status code is chosen by which case the handler returned -- and a handler answering with the wrong shape does
  * not compile. No handler in this module ever names a status.
  *
  * Each branch is converted to its own case before the union is converted to the sum, which is the shape
  * `fixture/json.scala`'s `verdict` already has. Doing that on the [[io.taig.otter.http.Result]] rather than inside the
  * body's schema is what keeps a named payload named: `schema.book` still reaches `components/schemas` as `Book`, where
  * converting the JSON record itself would have buried it.
  *
  * The conversion needs a branch that carries a body. `.to` is found through the `Profunctor` for
  * `Result.Schema[S, ?, ?]`, and a result with no entity has `S = Nothing`, which does not eta-expand to the kind the
  * instance asks for. So an answer with no entity stays a `Unit` in the union's `Either`, and what can still be
  * converted is the union as a whole: [[books.fetch]] maps `Either[Book, Unit]` onto `Option[Book]` with neither branch
  * named. A sum is worth reaching for once every branch has something to say, and [[loans.borrow]] is the three branch
  * case.
  */
enum Created:
  case Added(book: Book)
  case Duplicate(problem: Problem)

/** Everything about a book. */
object books:
  /** `/books`, written the way a URL is written.
    *
    * `/` is `:*` under another name and with a narrower type -- the same instances answer both -- so this is one schema
    * built by one code path rather than a second spelling to keep in step. A literal needs no `segment` around it:
    * there is nothing left to say about a position holding a fixed piece of text.
    */
  val all: Path[Unit] = __ / "books"

  /** `/books/{isbn}`, whose placeholder parses rather than merely capturing: a segment that is not an ISBN is a bad
    * request and never reaches a handler.
    */
  val one: Path[Isbn] = __ / "books" / segment("isbn", codec("isbn", Isbn.parse, _.value))

  /** Four parameters, each a different thing a query string can do: one defaulted, one defaulted, one repeated, and one
    * that is a name with no value at all.
    *
    * `available` is the interesting one. `?available` and `?available=` are both a name carrying no text, and a lenient
    * parameter reads that as absence before the value is looked at -- right for `?page=`, wrong here, where giving the
    * name *is* the assertion. `strict` is what lets the empty text through to be read as `true`.
    */
  val filter: Queries[(Int, Int, List[Genre], Boolean)] =
    query("page", int).optional(1) :*
      query("size", int).optional(20) :*
      query("genre", collection.list(enumerated)) :*
      query("available", coerce(boolean)).strict.optional(false)

  /** One header that has to be there and one list valued one that need not be.
    *
    * This is as far as an endpoint description goes towards authentication: there is no security scheme vocabulary in
    * this library, and a header a handler reads is the honest whole of what can be said.
    */
  val tracing: Headers[(String, Option[List[String]])] =
    header("X-Request-Id", string) :* header("Accept-Language", collection.list(string)).optional

  /** The genre spelling the query string uses, which is the JSON one: an enumeration is a mapping and the mapping does
    * not change with the position it is read in.
    */
  private def enumerated: Parameter.Enumeration[Genre] =
    dsl.enumeration[Parameter.Primitive.Text.Schema, String, Genre](dsl.string):
      case Genre.Biography => "biography"
      case Genre.Children  => "children"
      case Genre.Fantasy   => "fantasy"
      case Genre.History   => "history"
      case Genre.Poetry    => "poetry"
      case Genre.Romance   => "romance"
      case Genre.Thriller  => "thriller"

  /** `GET /books`
    *
    * The input is a five tuple whose last member is a pair, and not a flat six, which is worth reading twice:
    * [[io.taig.otter.Append]] appends what it is given as *one* member rather than concatenating it. The query string
    * contributes four values and the header set contributes one pair. `++` is the operator that concatenates instead,
    * and neither is a mistake for the other -- a header set is not four more query parameters.
    */
  val list: Endpoint[(Int, Int, List[Genre], Boolean, (String, Option[List[String]])), List[Book]] = endpoint(
    request(method.get, books.all).queries(books.filter).headers(books.tracing),
    result(code.ok)(body.json(json.collection.list(schema.book)))
  ).attr(openapi.operationId, "listBooks")
    .attr(openapi.summary, "Every book the catalogue holds")
    .attr(openapi.tags, "books")

  /** `POST /books`, answering with the book or with the reason it is already there.
    *
    * Both branches carry a body, which is what a two branch union is for: the answer is not "a book or nothing" but "a
    * book or a problem", and a caller reads which by the status code rather than by inspecting the document.
    */
  val create: Endpoint[Book.Create, Created] = endpoint(
    request(method.post, books.all)(body.json(schema.create)),
    (result(code.created)(body.json(schema.book)).to[Created.Added] :+
      result(code.conflict)(body.json(schema.problem)).to[Created.Duplicate]).to[Created]
  ).attr(openapi.operationId, "createBook")
    .attr(openapi.summary, "Add a book to the catalogue")
    .attr(openapi.tags, "books")

  /** `GET /books/{isbn}`, whose two answers are a book and no book -- which is what `Option` is.
    *
    * Nothing is named here, where [[Created]] names every case: the union is the same `200` beside a `404` it would be
    * anyway, and `.to` maps the `Either[Book, Unit]` it holds onto the type both ends of the wire hold already --
    * `Library.fetch` looks a book up in a `Map` and a caller wants an `Option` back. Without it each end pads and
    * unpads, in opposite directions, around a shape neither of them wants.
    *
    * The document is untouched by it. A conversion is a [[io.taig.otter.http.Result.Value.Modify]], which is what the
    * handler reads and not what the endpoint describes -- the OpenAPI and TypeScript renderings say `200` and `404`
    * either way.
    */
  val fetch: Endpoint[Isbn, Option[Book]] = endpoint(
    request(method.get, books.one),
    (result(code.ok)(body.json(schema.book)) :+ result(code.notFound)).to[Option[Book]]
  ).attr(openapi.operationId, "fetchBook")
    .attr(openapi.tags, "books")

  /** `PATCH /books/{isbn}`, whose body is where the two sides of one schema differ most. */
  val patch: Endpoint[(Isbn, Book.Patch), Option[Book]] = endpoint(
    request(method.patch, books.one)(body.json(schema.patch)),
    (result(code.ok)(body.json(schema.book)) :+ result(code.notFound)).to[Option[Book]]
  ).attr(openapi.operationId, "patchBook")
    .attr(openapi.tags, "books")

  /** `DELETE /books/{isbn}`, which is idempotent: a book that is not there is already gone, and 204 is the honest
    * answer rather than a 404. What it cannot do is remove a book somebody is holding, and that is the conflict.
    *
    * The one two branch answer left as a plain `Either`, and the contrast [[books.fetch]] is worth reading against: the
    * branch carrying nothing here is the *success*, so `Option[Problem]` would name the failure as the thing that is
    * present and get the endpoint exactly backwards. Not a sum either -- see [[Created]] for why a branch with no
    * entity cannot be converted on its own.
    */
  val delete: Endpoint[Isbn, Either[Unit, Problem]] = endpoint(
    request(method.delete, books.one),
    result(code.noContent) :+ result(code.conflict)(body.json(schema.problem))
  ).attr(openapi.operationId, "deleteBook")
    .attr(openapi.tags, "books")

  /** `POST /books/{isbn}/scan`: bytes in, bytes out, and no document anywhere in it.
    *
    * A body need not have a schema. `body.binary` says only what the media type is, and what crosses is a `ByteVector`
    * -- which is how a PDF, an image or anything else opaque is described without pretending it has structure.
    */
  val scan: Endpoint[(Isbn, ByteVector), ByteVector] = endpoint(
    request(method.post, books.one / "scan")(body.binary(mediaType.pdf)),
    result(code.ok)(body.binary(mediaType.pdf))
  ).attr(openapi.operationId, "scanBook")
    .attr(openapi.tags, "books")

  /** A body that may be either of two things, told apart by media type and not by trying to parse each in turn. */
  val submitted: Bodies[Either[Book.Create, ByteVector]] = body.json(schema.create) :+ body.binary(mediaType.pdf)

  /** `POST /intake`: an acquisition, sent as a document or as a scan of the paperwork, or announced with neither.
    *
    * Both halves of a body's optionality at once. `bodies` is the choice between alternatives, told apart by media type
    * rather than by trying to parse each in turn; `optionalBodies` is the further fact that the whole entity may not be
    * sent at all, which is a different thing from sending an empty one. The two compose into the `Option[Either[...]]`
    * the handler reads, and a notice that a shipment is coming needs no attachment.
    */
  val intake: Endpoint[Option[Either[Book.Create, ByteVector]], Unit] = endpoint(
    request(method.post, __ / "intake")(body.optional(books.submitted)),
    result(code.accepted)
  ).attr(openapi.operationId, "intake")
    .attr(openapi.tags, "books")

  /** A JSON part and a file part, one of which need not be sent.
    *
    * A multipart payload is a *product* of bodies, which is what makes an upload describable at all: each part carries
    * its own media type, and the file part carries the name it claims to have been saved under. A part is a field, so
    * it carries a field's optionality too.
    */
  val cover: Multipart[(Book.Patch, Option[ByteVector])] =
    part("metadata", body.json(schema.patch)) :*
      part("image", body.binary(mediaType.octetStream)).filename("cover.png").optional

  /** `POST /books/{isbn}/cover`. Described here, and served nowhere -- see [[api.unserved]]. */
  val upload: Endpoint[(Isbn, (Book.Patch, Option[ByteVector])), Unit] = endpoint(
    request(method.post, books.one / "cover")(body.multipart(books.cover)),
    result(code.noContent)
  ).attr(openapi.operationId, "uploadCover")
    .attr(openapi.tags, "books")

  /** `GET /books/export`, a sequence of books one JSON document per line.
    *
    * The element type rides on the body, so a backend handed it knows what its stream yields, and the body itself
    * contributes nothing to what the endpoint holds -- what a sequence of elements *is* belongs to whoever has an
    * effect type to say it in, and nothing in this module does. Described here and served nowhere: see
    * [[api.unserved]].
    */
  val exported: Endpoint[Unit, Unit] = endpoint(
    request(method.get, books.all / "export"),
    result(code.ok)(body.ndjson(schema.book))
  ).attr(openapi.operationId, "exportBooks")
    .attr(openapi.tags, "books")

  /** `GET /books/report`, a stream of CSV rows.
    *
    * The media type and the framing are spelled out where `body.ndjson` defaults them, and the payload is written in an
    * alphabet no interpreter in this repository recognises. Both halves of that are deliberate: a body's payload is any
    * schema at all, and what cannot be carried is reported rather than quietly dropped.
    */
  val report: Endpoint[Unit, Unit] = endpoint(
    request(method.get, books.all / "report"),
    result(code.ok)(body.streamed(mediaType.csv, Frame.Lines, schema.row))
  ).attr(openapi.operationId, "reportBooks")
    .attr(openapi.tags, "books")

  /** The catalogue as a tree of shelves, which is the endpoint the recursive schema exists for. */
  val catalogue: Endpoint[Unit, Category] = endpoint(
    request(method.get, __ / "catalogue"),
    result(code.ok)(body.json(schema.category))
  ).attr(openapi.operationId, "catalogue")
    .attr(Keys.description, "Shelves, and the shelves inside them, to any depth")
    .attr(openapi.tags, "catalogue")
