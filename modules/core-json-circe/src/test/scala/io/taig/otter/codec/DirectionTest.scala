package io.taig.otter.codec

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.otter.fixture.Book
import io.taig.otter.fixture.Isbn
import io.taig.otter.fixture.Note
import io.taig.otter.fixture.json
import zio.Scope
import zio.test.*

import scala.compiletime.testing.typeChecks

/** The point of carrying the direction in the type is that the compiler rejects the wrong one. */
object DirectionTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("DirectionTest")(
    test("a write only schema can be encoded"):
      assertTrue(typeChecks("""JsonCirceEncoder.encode(json.title, io.taig.otter.fixture.Book("a", 1, true))"""))
    ,
    test("a write only schema cannot be decoded"):
      assertTrue(!typeChecks("""val isbn: io.taig.otter.fixture.Isbn =
        JsonCirceDecoder.decode(json.title, io.circe.Json.Null).getOrElse(???)"""))
    ,
    test("a dictionary key is text, so a number schema cannot name one"):
      assertTrue(
        typeChecks("""dictionary.map(json.counter, string)"""),
        !typeChecks("""dictionary.map(int, string)""")
      )
    ,
    test("a dictionary whose key can only be read can only be decoded"):
      assertTrue(
        typeChecks("""JsonCirceDecoder.decode(json.catalogue, io.circe.Json.obj())"""),
        !typeChecks("""JsonCirceEncoder.encode(json.catalogue, Nil)""")
      )
    ,
    test("a read only schema can be decoded"):
      assertTrue(typeChecks("""JsonCirceDecoder.decode(json.isbn, io.circe.Json.fromString("978"))"""))
    ,
    test("a read only schema cannot be encoded"):
      assertTrue(!typeChecks("""JsonCirceEncoder.encode(json.isbn, io.taig.otter.fixture.Isbn("978"))"""))
    ,
    test("a round tripping schema works in both directions"):
      assertTrue(
        typeChecks("""JsonCirceEncoder.encode(json.book, io.taig.otter.fixture.Book("a", 1, true))"""),
        typeChecks("""JsonCirceDecoder.decode(json.book, io.circe.Json.Null)""")
      )
    ,
    test("map leaves a reader, not a round trip"):
      assertTrue(
        typeChecks("""val schema: Json.Primitive.Text.Reader[String] = string.map(_.reverse)"""),
        !typeChecks("""val schema: Json.Primitive.Text[String] = string.map(_.reverse)""")
      )
    ,
    test("a mapped schema cannot be encoded"):
      assertTrue(!typeChecks("""JsonCirceEncoder.encode(string.map(_.reverse), "a")"""))
    ,
    test("contramap leaves a writer, not a round trip"):
      assertTrue(
        typeChecks("""val schema: Json.Primitive.Text.Writer[Book] = string.contramap[Book](_.title)"""),
        !typeChecks("""val schema: Json.Primitive.Text[Book] = string.contramap[Book](_.title)""")
      )
    ,
    test("a contramapped schema cannot be decoded"):
      assertTrue(!typeChecks("""val title: String =
        JsonCirceDecoder.decode(string.contramap[Book](_.title), io.circe.Json.Null).getOrElse(???)"""))
    ,
    test("a reader is a Functor"):
      val schema = Functor[[a] =>> Json.Primitive.Text.Schema[Nothing, a]].map(json.isbn)(_.value)
      assertTrue(JsonCirceDecoder.decode(schema, CirceJson.fromString("978")) == Validated.valid("978"))
    ,
    test("a writer is a Contravariant"):
      val schema = Contravariant[[a] =>> Json.Primitive.Text.Schema[a, Any]]
        .contramap(json.title)((isbn: Isbn) => Book(isbn.value, 1, true))
      assertTrue(JsonCirceEncoder.encode(schema, Isbn("978")) == CirceJson.fromString("978"))
    ,
    test("a round tripping schema is an Invariant"):
      val schema = Invariant[[a] =>> Json.Record.Schema[Json.Node, a, a]]
        .imap(json.book)(_.title)(Book(_, 1, true))
      val encoded = JsonCirceEncoder.encode(schema, "Dune")
      assertTrue(JsonCirceDecoder.decode(schema, encoded) == Validated.valid("Dune"))
    ,
    /** Normalising is not the same as losing a direction, and the type should not conflate them: `map` forgets the
      * write side, `normalized` keeps it.
      */
    test("a normalised schema can be encoded, where a mapped one cannot"):
      assertTrue(
        typeChecks("""JsonCirceEncoder.encode(json.trimmed, " foo ")"""),
        !typeChecks("""JsonCirceEncoder.encode(string.map(_.trim), " foo ")""")
      )
    ,
    /** The write side is the raw wire text, not the read side's inverse. Encoding leaves the text alone and decoding
      * normalises it, so the two together are not the identity on a document -- which is the point, because it is what
      * lets a caller submit text the read side has to clean up.
      */
    test("a normalised schema writes the raw text and reads its normal form"):
      val encoded = JsonCirceEncoder.encode(json.trimmed, " foo ")
      assertTrue(
        encoded == CirceJson.fromString(" foo "),
        JsonCirceDecoder.decode(json.trimmed, encoded) == Validated.valid("foo")
      )
    ,
    test("a record holding a normalised field still writes"):
      val encoded = JsonCirceEncoder.encode(json.trimmedNote, Note(" Dune ", 1.some))
      assertTrue(
        encoded == CirceJson.obj("title" -> CirceJson.fromString(" Dune "), "tag" -> CirceJson.fromInt(1)),
        JsonCirceDecoder.decode(json.trimmedNote, encoded) == Validated.valid(Note("Dune", 1.some))
      )
    ,
    /** `parser` builds a node whose print half is `identity[String]`, so the text it parses is writable. Only the
      * ascription takes that away, which is what `json.isbn` does and what the negative above pins.
      */
    test("a parser writes the text it parses"):
      val schema = parser("isbn", value => Right(Isbn(value)))
      val encoded = JsonCirceEncoder.encode(schema, "978")
      assertTrue(
        encoded == CirceJson.fromString("978"),
        JsonCirceDecoder.decode(schema, encoded) == Validated.valid(Isbn("978"))
      )
    ,
    /** Appending a child that only reads leaves `Append` unable to reduce the write side, because Scala does not reduce
      * a match type over `Nothing`. The stuck type is harmless: `Nothing` conforms to it, so the reader ascription
      * absorbs it and the direction that does exist still works.
      */
    test("a record appending a read only child still reads"):
      val schema: Json.Record.Reader[DirectionTest.Catalogue] =
        (field("title", string) :* field("isbn", json.isbn)).mapTo
      val document = CirceJson.obj("title" -> CirceJson.fromString("Dune"), "isbn" -> CirceJson.fromString("978"))
      assertTrue(
        JsonCirceDecoder.decode(schema, document) == Validated.valid(DirectionTest.Catalogue("Dune", Isbn("978")))
      )
    ,
    /** The same, with the read only child first, so that its `Nothing` lands in the left slot instead of the right. */
    test("a record appending a read only child first still reads"):
      val schema: Json.Record.Reader[DirectionTest.Listing] =
        (field("isbn", json.isbn) :* field("title", string)).mapTo
      val document = CirceJson.obj("isbn" -> CirceJson.fromString("978"), "title" -> CirceJson.fromString("Dune"))
      assertTrue(
        JsonCirceDecoder.decode(schema, document) == Validated.valid(DirectionTest.Listing(Isbn("978"), "Dune"))
      )
  )

  final private case class Catalogue(title: String, isbn: Isbn)

  final private case class Listing(isbn: Isbn, title: String)
