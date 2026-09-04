package io.taig.otter.fixture

import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*

import java.util.UUID
import scala.collection.immutable.SortedMap

object json:
  val book: Json.Record[Book] =
    (field("title", string) :* field("pages", int) :* field("read", boolean)).to

  /** The same note twice: once dropping the key when the tag is absent, once writing an explicit null. */
  val omittedTag: Json.Record[Note] = (field("title", string) :* field("tag", int).optional).to

  val nullableTag: Json.Record[Note] = (field("title", string) :* field("tag", int).optional.nullable).to

  /** Two layers of absence, which only a strict field can tell apart: no key at all is the outer one, a null is the
    * inner one.
    */
  val nestedTag: Json[Option[Option[Int]]] = field("tag", int.optional).optional.omitted.strict.toRecord

  val genre: Json.Enumeration[Genre] = enumeration(string):
    case Genre.Fiction => "fiction"
    case Genre.History => "history"
    case Genre.Poetry  => "poetry"

  val circle: Json.Record[Shape.Circle] = field("radius", double).toRecord.to

  val square: Json.Record[Shape.Square] = field("side", double).toRecord.to

  val triangle: Json.Record[Shape.Triangle] =
    (field("base", double) :* field("height", double)).to

  /** Three branches, so the union nests two levels deep. */
  val shape: Json.Union[Shape] =
    (branch("circle", circle) :+ branch("square", square) :+ branch("triangle", triangle)).to

  /** How a [[Tag]] is written where a name is expected. Only the write side is ever reachable, which is honest: a name
    * is printed where the schema is built and never reaches the wire.
    */
  val tag: Json.Primitive.Text.Writer[Tag] = printer("tag", _.toString.toLowerCase)

  /** The same union as [[shape]], every branch named by a [[Tag]] rather than by a literal. Naming is the only
    * difference, so the two schemas read and write alike.
    */
  val taggedShape: Json.Union[Shape] =
    (branch(Tag.Circle, tag, circle) :+ branch(Tag.Square, tag, square) :+ branch(Tag.Triangle, tag, triangle)).to

  /** A union whose two cases without members read a singleton type, which is widened on the way in and leaves those
    * branches indistinguishable. The write side keeps them apart, so this still round trips under the same `to` every
    * other union is written with.
    */
  val verdict: Json.Union[Verdict] = (
    branch("accepted", field("type", constant(string, "accepted")).toRecord.to[Verdict.Accepted.type]) :+
      branch("rejected", field("type", constant(string, "rejected")).toRecord.to[Verdict.Rejected.type]) :+
      branch(
        "deferred",
        (field("type", constant(string, "deferred")) :* field("reason", string)).to[Verdict.Deferred]
      )
  ).to[Verdict]

  /** An integer carried as text. JSON has no numeric keys, so an integer key is a named format the way [[isbn]] is,
    * rather than `int`, which would claim the document holds a number.
    */
  val counter: Json.Primitive.Text[Int] = codec("int", _.toIntOption.toRight("not an int"), _.toString)

  /** A dictionary whose keys are parsed rather than taken verbatim. */
  val editions: Json.Dictionary[SortedMap[UUID, Int]] = dictionary.map(uuid, int)

  val printings: Json.Dictionary[List[(Int, String)]] = dictionary.list(counter, string)

  /** A dictionary whose keys can be read but not written, the way [[isbn]] cannot be written.
    *
    * Lazy because [[isbn]] is declared below it: a `val` here captures the forward reference as `null`, and a
    * dictionary only forces its key schema once it has a key to read, so the failure waits for the first document that
    * actually holds one.
    */
  lazy val catalogue: Json.Dictionary.Reader[List[(Isbn, String)]] = dictionary.list(json.isbn, string)

  lazy val tree: Json.Record[Tree] = (
    field("value", int) :*
      field("children", collection.list(tree))
  ).to

  /** The same three fields, ascribed to say that every one of them is a primitive. That is what a flat format can
    * represent, and it is a compile error to write this down for a schema that nests.
    */
  val flatBook: Json.Record.Of[Json.Primitive.Node, Book] =
    (field("title", string) :* field("pages", int) :* field("read", boolean)).to

  /** Can be written but not read: there is no way back from a title to a book. */
  val title: Json.Primitive.Text.Writer[Book] = printer("title", _.title)

  /** Can be read but not written. */
  val isbn: Json.Primitive.Text.Reader[Isbn] = parser("isbn", value => Right(Isbn(value)))

  /** Normalised on the way in, written back verbatim.
    *
    * Spelled out rather than given the round tripping `Json.Primitive.Text[String]` alias, because the two `String`s
    * are not the same one: the write side is the raw wire text, the read side is its normal form.
    */
  val trimmed: Json.Primitive.Text.Schema[String, String] = normalized("trimmed", _.trim)

  /** A record holding a normalised field still writes. Spelled out for the same reason [[trimmed]] is. */
  val trimmedNote: Json.Record.Schema[Json.Node, Note, Note] =
    (field("title", trimmed) :* field("tag", int).optional).to

  /** Can be written but not read, the way [[title]] cannot be read. */
  val label: Json.Primitive.Text.Writer[Isbn] = printer("label", _.value)

  /** A member that can only be written, with members after it. */
  val shelf: Json.Record.Writer[Shelf] =
    (field("label", json.label) :* field("pages", int) :* field("read", boolean)).contramapTo

  val census: Json.Record[Census] = (
    field("first", string) :*
      field("second", string) :*
      field("third", string) :*
      field("fourth", string) :*
      field("fifth", string) :*
      field("sixth", string) :*
      field("seventh", string) :*
      field("eighth", string) :*
      field("ninth", string) :*
      field("tenth", string) :*
      field("eleventh", string) :*
      field("twelfth", string) :*
      field("thirteenth", string) :*
      field("fourteenth", string) :*
      field("fifteenth", string)
  ).to
